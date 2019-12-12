! { dg-do compile }
! { dg-require-effective-target vect_double }
! { dg-additional-options "-finline-matmul-limit=0 --param vect-epilogues-nomask=1" }
! { dg-additional-options "-mstrict-align" { target { aarch64*-*-* } } }

#include "vect-8.f90"
