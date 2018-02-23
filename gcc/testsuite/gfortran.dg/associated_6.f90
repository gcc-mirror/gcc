! { dg-do run }
!
! PR 54881: [4.8 Regression] [OOP] ICE in fold_convert_loc, at fold-const.c:2016
!
! Contributed by Richard L Lozes <richard@lozestech.com>

  implicit none

  type treeNode
    type(treeNode), pointer :: right => null()
  end type

  type(treeNode) :: n

  if (associated(RightOf(n))) STOP 1
  allocate(n%right)
  if (.not.associated(RightOf(n))) STOP 2
  deallocate(n%right)
  
contains

  function RightOf (theNode)
    class(treeNode), pointer :: RightOf
    type(treeNode), intent(in) :: theNode
    RightOf => theNode%right
  end function
  
end
