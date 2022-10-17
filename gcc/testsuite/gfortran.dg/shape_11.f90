! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR fortran/103610 - ICE while simplifying SHAPE
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(-1) = 1
  integer, parameter :: b(1)  = maskl(shape(a))
  integer, parameter :: c(1)  = shape(a)
  integer, parameter :: d(1)  = maskr(shape(a))
  if (b(1) /= 0) stop 1
  if (c(1) /= 0) stop 2
  if (d(1) /= 0) stop 3
end

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
