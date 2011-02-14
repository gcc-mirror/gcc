! { dg-do compile }
!
! PR 47330: [OOP] ICE on invalid source in connection with SELECT TYPE
!
! Contributed by Andrew Benson <abenson@its.caltech.edu>

  type treeNode
  end type
contains
  subroutine proc1 (thisNode)
    class (treeNode), target :: thisNode
    select type (thisNode)
    type is (treeNode)
       workNode => thisNode  ! { dg-error "Non-POINTER in pointer association context" }
    end select
  end subroutine
end
