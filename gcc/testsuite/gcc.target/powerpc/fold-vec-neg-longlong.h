/* Verify that overloaded built-ins for vec_neg with long long
   inputs produce the right code.  */

/* vec_neg testcase, included by fold-vec-neg-longlong.p*.c */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed long long
test3 (vector signed long long x)
{
  return vec_neg (x);
}

