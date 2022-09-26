/* Test that we use vpdi in order to reverse vectors
   with two elements instead of creating a literal-pool entry
   and permuting with vperm.  */
/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O2 -march=z14 -mzarch -mzvector -fno-unroll-loops" } */

/* { dg-final { scan-assembler-times "vpdi\t" 4 } } */
/* { dg-final { scan-assembler-times "verllg\t" 2 } } */
/* { dg-final { scan-assembler-times "vperm" 0 } } */

#include <vecintrin.h>

vector double reved (vector double a)
{
   return vec_reve (a);
}

vector long long revel (vector long long a)
{
   return vec_reve (a);
}

vector float revef (vector float a)
{
   return vec_reve (a);
}

vector int revei (vector int a)
{
   return vec_reve (a);
}
