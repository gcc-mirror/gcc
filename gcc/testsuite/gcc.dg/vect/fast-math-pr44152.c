/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

_Complex float a[N]  __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) =
    { 10.0F + 20.0iF, 11.0F + 21.0iF, 12.0F + 22.0iF, 13.0F + 23.0iF,
      14.0F + 24.0iF, 15.0F + 25.0iF, 16.0F + 26.0iF, 17.0F + 27.0iF,
      18.0F + 28.0iF, 19.0F + 29.0iF, 20.0F + 30.0iF, 21.0F + 31.0iF,
      22.0F + 32.0iF, 23.0F + 33.0iF, 24.0F + 34.0iF, 25.0F + 35.0iF };

_Complex float c[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));


__attribute__ ((noinline)) _Complex float 
foo (int x)
{
  int i;
  _Complex float *p = a + x;
  _Complex float sum = 10.0F + 20.0iF;
 
  for (i = 0; i < N; i++)
   {
     sum += *p;
     p++;
   }

  c[0] = sum + 66.0F + 86.0iF; 
 
  return 0;
}


