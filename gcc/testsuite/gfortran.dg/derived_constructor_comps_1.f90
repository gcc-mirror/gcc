! { dg-do run }
!
! Tests fix for PR28425 in which anything other than a constructor would
! not work for derived type components in a structure constructor.
!
! Original version sent by Vivek Rao on 18 Jan 06
! Modified by Steve Kargl to remove IO
!
module foo_mod

  implicit none

  type :: date_m
     integer :: month
  end type date_m

  type :: file_info
     type(date_m) :: date
  end type file_info

end module foo_mod

program prog

  use foo_mod

  implicit none
  type(date_m)  :: dat
  type(file_info) :: xx

  type(date_m), parameter :: christmas = date_m (12)

  dat = date_m(1)

  xx = file_info(date_m(-1))  ! This always worked - a constructor
  if (xx%date%month /= -1) STOP 1

  xx = file_info(dat)         ! This was the original PR - a variable
  if (xx%date%month /= 1) STOP 2

  xx = file_info(foo(2))      ! ...functions were also broken
  if (xx%date%month /= 2) STOP 3

  xx = file_info(christmas)   ! ...and parameters
  if (xx%date%month /= 12) STOP 4


contains

  function foo (i) result (ans)
     integer :: i
     type(date_m) :: ans
     ans = date_m(i)
  end function foo

end program prog
