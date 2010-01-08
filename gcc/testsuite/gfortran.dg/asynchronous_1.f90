! { dg-do compile }
!
! PR/fortran 25829
!
! Check parsing and checking of ASYNCHRONOUS
!
type(t) function func0()
  asynchronous :: a
  integer, asynchronous:: b
  allocatable :: c
  volatile :: d
  type t
    sequence
    integer :: i = 5
  end type t
end function func0

integer function func()
  asynchronous :: func
  integer, asynchronous:: b
  allocatable :: c
  volatile :: func
  type t
    sequence
    integer :: i = 5
  end type t
end function func

function func2() result(res)
  volatile res
  asynchronous res
end function func2

subroutine sub()
  asynchronous sub ! { dg-error "SUBROUTINE attribute conflicts with ASYNCHRONOUS" }
  volatile sub     ! { dg-error "SUBROUTINE attribute conflicts with VOLATILE" }
end subroutine sub

program main
  asynchronous main ! { dg-error "PROGRAM attribute conflicts with ASYNCHRONOUS" }
  volatile main     ! { dg-error "PROGRAM attribute conflicts with VOLATILE" }
end program main
