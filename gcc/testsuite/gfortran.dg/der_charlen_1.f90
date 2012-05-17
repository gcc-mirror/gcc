! { dg-do compile }
! PR 18990
! we used to ICE on these examples
module core
  type, public  :: T
     character(len=I)  :: str ! { dg-error "needs to be a constant specification expression" }
  end type T
  private
CONTAINS
  subroutine FOO(X)
    type(T), intent(in)          :: X
  end subroutine
end module core

module another_core
  type :: T
     character(len=*)  :: s ! { dg-error "needs to be a constant specification expr" }
  end type T
  private
CONTAINS
  subroutine FOO(X)
    type(T), intent(in)          :: X
  end subroutine
end module another_core
