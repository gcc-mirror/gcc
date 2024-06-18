! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }
!
! PR fortran/115390 - fixes for CHARACTER(len=*) dummies with bind(C)

module test
  implicit none
contains
  subroutine bar(s,t) bind(c)
    character(*), intent(in) :: s,t
    optional                 :: t
    call foo(s,t)
  end
  subroutine bar1(s,t) bind(c)
    character(*), intent(in) :: s(:),t(:)
    optional                 :: t
    call foo1(s,t)
  end
  subroutine bar4(s,t) bind(c)
    character(len=*,kind=4), intent(in) :: s,t
    optional                            :: t
    call foo4(s,t)
  end
  subroutine bar5(s,t) bind(c)
    character(len=*,kind=4), intent(in) :: s(:),t(:)
    optional                            :: t
    call foo5(s,t)
  end
  subroutine foo(s,t)
    character(*), intent(in) :: s,t
    optional                 :: t
  end
  subroutine foo1(s,t)
    character(*), intent(in) :: s(:),t(:)
    optional                 :: t
  end
  subroutine foo4(s,t)
    character(len=*,kind=4), intent(in) :: s,t
    optional                            :: t
  end
  subroutine foo5(s,t)
    character(len=*,kind=4), intent(in) :: s(:),t(:)
    optional                            :: t
  end
end
