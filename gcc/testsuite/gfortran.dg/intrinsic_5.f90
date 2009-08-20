! { dg-do compile }
! { dg-options "-fimplicit-none" }
!
! PR 41121: [4.5 Regression] compile-time error when building BLAS with -fimplicit-none
!
! Original test case: http://www.netlib.org/blas/dgbmv.f
! Reduced by Joost VandeVondele <jv244@cam.ac.uk>

  INTRINSIC MIN
  INTEGER :: I,J
  print *,MIN(I,J)
END

