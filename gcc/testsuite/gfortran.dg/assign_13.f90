! { dg-do run }
!
! PR fortran/121185
! The assignment to Y%X in CHECK_T was using a polymorphic array access on the
! left hand side, using the virtual table of Y.

program p
  implicit none
  type t
     complex, allocatable :: x(:)
  end type t
  real :: trace = 2.
  type(t) :: z
  z%x = [1,2] * trace
  call check_t (z)
contains
  subroutine check_t (y)
    class(t) :: y
    ! print *, y% x
    if (any(y%x /= [2., 4.])) error stop 11
    y%x = y%x / trace
    ! print *, y% x
    if (any(y%x /= [1., 2.])) error stop 12
  end subroutine
end
