! { dg-do run }
!
! PR 47767: [OOP] SELECT TYPE fails to execute correct TYPE IS block
!
! Contributed by Andrew Benson <abenson@caltech.edu>

module Tree_Nodes
  type treeNode
   contains
     procedure :: walk
  end type
contains
  subroutine walk (thisNode)
    class (treeNode) :: thisNode
    print *, SAME_TYPE_AS (thisNode, treeNode())
  end subroutine
end module

module Merger_Trees
  use Tree_Nodes
  private
  type(treeNode), public :: baseNode
end module

module Merger_Tree_Build
  use Merger_Trees
end module

program test
  use Merger_Tree_Build
  use Tree_Nodes
  type(treeNode) :: node
  call walk (node)
end program
