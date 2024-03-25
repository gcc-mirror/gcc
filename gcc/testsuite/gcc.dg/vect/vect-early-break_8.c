/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

#include <complex.h>

#define N 1024
char vect_a[N];
char vect_b[N];
  
char test4(char x, char * restrict res)
{
 char ret = 0;
 for (int i = 0; i < N; i++)
 {
   vect_b[i] += x + i;
   if (vect_a[i] > x)
     break;
   vect_a[i] += x * vect_b[i];
   res[i] *= vect_b[i];
 }
 return ret;
}
