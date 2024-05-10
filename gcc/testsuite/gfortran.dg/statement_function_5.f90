! { dg-do compile }
! PR fortran/115039
!
! Check that inquiry refs work with statement functions
!
! { dg-additional-options "-std=legacy -fdump-tree-optimized" }
! { dg-prune-output " Obsolescent feature" }
! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "optimized" } }

program testit
  implicit none
  complex :: x
  real    :: im
  integer :: slen
  character(5) :: s
  im(x)   = x%im + x%re + x%kind
  slen(s) = s%len
  if (im((1.0,3.0) + (2.0,4.0)) /= 14.) stop 1
  if (slen('abcdef') /= 5)              stop 2
end program testit
