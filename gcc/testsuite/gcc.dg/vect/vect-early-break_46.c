/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_float } */

#include <complex.h>

#define N 1024
complex double vect_a[N];
complex double vect_b[N];
  
complex double test4(complex double x)
{
 complex double ret = 0;
 for (int i = 0; i < N; i++)
 {
   vect_b[i] += x + i;
   if (vect_a[i] == x)
     return i;
   vect_a[i] += x * vect_b[i];
   
 }
 return ret;
}

/* At -O2 we can't currently vectorize this because of the libcalls not being
   lowered.  */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect"  { xfail *-*-* } } } */