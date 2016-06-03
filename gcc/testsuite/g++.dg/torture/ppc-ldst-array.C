/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-skip-if "do not override mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8" } */

/* When compiled with C++, this code was breaking because of different
   tree representations of arrays between C and C++.  */

#include <altivec.h>

extern vector float vf;

void foo ()
{
  float __attribute__((aligned (16))) x[4];
  float __attribute__((aligned (16))) y[4];
  vf = vec_ld (0, x);
  vec_st (vf, 0, y);
}
