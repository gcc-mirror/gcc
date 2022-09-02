! { dg-do run }
!
! Test the fix for PR100245
!

program main_p

  implicit none

  type :: foo_t
    integer :: a
  end type foo_t

  integer, parameter :: a = 42

  class(foo_t), allocatable :: val
  class(foo_t), allocatable :: rs1
  type(foo_t),  allocatable :: rs2

  allocate(val, source=foo_t(42))
  if (val%a/=a) stop 1
  rs1 = val
  if (rs1%a/=a) stop 2
  rs2 = val
  if (rs2%a/=a) stop 3
  deallocate(val, rs1, rs2)

end program main_p
