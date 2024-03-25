/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { ! "arm*-*-*" } } } } */

#include <complex.h>

#define N 1024
complex double vect_a[N];
complex double vect_b[N];
  
complex double test4(complex double x, complex double t)
{
 complex double ret = 0;
 for (int i = 0; i < N; i++)
 {
   vect_a[i] = t + i;
   if (vect_a[i] == x)
     return i;
   vect_a[i] += x * vect_a[i];
   
 }
 return ret;
}
