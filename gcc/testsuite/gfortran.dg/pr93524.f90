! { dg-additional-sources pr93524.c }
! { dg-do run }
!
! Test the fix for PR93524.  The main program is in pr93524.c.

subroutine my_fortran_sub_1 (A) bind(C)
  real :: A(:, :, :)
  if (any (lbound(A) /= 1)) stop 1
  if (any (ubound(A) /= [21,6,8])) stop 2
  if (.not. is_contiguous (A)) stop 3
end
subroutine my_fortran_sub_2 (A) bind(C)
  real, ALLOCATABLE :: A(:, :, :)
  if (any (lbound(A) /= [-10,0,3])) stop 1
  if (any (ubound(A) /= [10,5,10])) stop 2
  if (.not. is_contiguous (A)) stop 3
end subroutine my_fortran_sub_2
