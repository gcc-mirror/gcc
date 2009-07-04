! { dg-do run }
!
! PR 40593: Proc-pointer returning function as actual argument
!
! Original test case by Tobias Burnus <burnus@gcc.gnu.org>
! Modified by Janus Weil

module m
contains
  subroutine sub(a)
    integer :: a
    a = 42
  end subroutine
  integer function func()
    func = 42
  end function
end module m

program test
  use m
  implicit none
  call caller1(getPtr1())
  call caller2(getPtr2())
  call caller3(getPtr2())
contains
  subroutine caller1(s)
    procedure(sub) :: s
    integer :: b
    call s(b)
    if (b /= 42)  call abort()
  end subroutine
  subroutine caller2(f)
    procedure(integer) :: f
    if (f() /= 42)  call abort()
  end subroutine
  subroutine caller3(f)
    procedure(func),pointer :: f
    if (f() /= 42) call abort()
  end subroutine
  function getPtr1()
    procedure(sub), pointer :: getPtr1
    getPtr1 => sub
  end function
  function getPtr2()
    procedure(func), pointer :: getPtr2
    getPtr2 => func
  end function
end program test

! { dg-final { cleanup-modules "m" } }

