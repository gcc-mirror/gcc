! PR fortran/88902
! { dg-do compile }
! { dg-require-effective-target lto }
! { dg-options "-flto --param ggc-min-heapsize=0" }

include 'pr50069_2.f90'
