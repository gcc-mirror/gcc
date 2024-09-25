! { dg-do run }
!
! Check that the code necessary to evaluate MINLOC's or MAXLOC's MASK
! argument is correctly generated.

program p
  implicit none
  integer, parameter :: data10(*) = (/ 2, 5, 2, 0, 6, 5, 3, 6, 0, 1 /)
  logical, parameter :: mask10(*) = (/ .false., .true., .false., &
                                       .false., .true., .true.,  &
                                       .true. , .true., .false., &
                                       .false. /)
  type bool_wrapper
    logical :: l
  end type
  call check_minloc
  call check_maxloc
contains
  subroutine check_minloc
    integer :: a(10)
    integer :: r
    a = data10
    r = minloc(a, dim = 1, mask = sum(a) > 0)
    if (r /= 4) stop 11
  end subroutine
  subroutine check_maxloc
    integer :: a(10)
    integer :: r
    a = data10
    r = maxloc(a, dim = 1, mask = sum(a) > 0)
    if (r /= 5) stop 18
  end subroutine
end program
