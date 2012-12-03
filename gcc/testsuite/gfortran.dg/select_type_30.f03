! { dg-do compile }
!
! PR 54881: [4.8 Regression] [OOP] ICE in fold_convert_loc, at fold-const.c:2016
!
! Contributed by Richard L Lozes <richard@lozestech.com>

  implicit none

  type treeNode
  end type

  class(treeNode), pointer :: theNode
  logical :: lstatus
  
  select type( theNode )
  type is (treeNode)
    call DestroyNode (theNode, lstatus )
  class is (treeNode)
    call DestroyNode (theNode, lstatus )
  end select
  
contains

  subroutine DestroyNode( theNode, lstatus )
    type(treeNode), pointer :: theNode
    logical, intent(out) :: lstatus
  end subroutine
  
end 
