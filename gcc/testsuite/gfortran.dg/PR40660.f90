! { dg-do compile }
! { dg-options "-fdump-tree-original-lineno" }
!
! PR fortran/40660

PROGRAM test
  INTEGER, DIMENSION(3) :: a1,a2
  a1 = 1
  PRINT*, a1
  a2 = 2
end program test

! { dg-final { scan-tree-dump-times ": 3\] _gfortran" 0 "original" } }

