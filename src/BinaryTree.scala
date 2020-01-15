object BinaryTree {

  case class TreeNode(d: Int, leftNodes: Option[TreeNode] = None, rightNodes: Option[TreeNode] = None){
    def add(a: Int) = {
      insert(a)
    }
    def insert(a: Int): Option[TreeNode] = {
      def leftInsert(a: Int) = leftNodes.flatMap(_.insert(a)) orElse Option(TreeNode(a))
      def rightInsert(a: Int) = rightNodes.flatMap(_.insert(a)) orElse Option(TreeNode(a))
      a.compare(d) match {
        case 0 => Option(this)
        case -1 => Option(TreeNode(d, leftInsert(a), rightNodes))
        case 1 => Option(TreeNode(d, leftNodes, rightInsert(a)))
      }
    }

    def traverseItInorder(): Option[List[TreeNode]] = {
      val leftList = leftNodes.flatMap(r => r.traverseItInorder()).getOrElse(Nil)
      val rightList = rightNodes.flatMap(r => r.traverseItInorder()).getOrElse(Nil)
      val result = (leftList :+ this) ++ rightList
      Option(result)
    }

    def traverseInorder() = {
      val result = traverseItInorder()
      result.getOrElse(Nil)
    }
  }

  def main(args: Array[String]): Unit = {
    val tree = TreeNode(4).add(2).get.add(3).get.add(1).get.add(11).get.add(15)

    /**
      * prints : 1 2 3 4 11 15
      */
    for {
      r <- tree.get.traverseInorder()
    } print(r.d + " ")
  }

}
