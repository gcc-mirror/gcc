/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

#include <stdlib.h>
#include "../../tree-vect.h"

__attribute__ ((noinline)) void 
ggSpectrum_Set8(float * data, float d) 
{
   int i;

   /* PR92127, disable unroll to avoid unexpected profit calculation.  */
   #pragma GCC unroll 0
   for (i = 0; i < 8; i++)
      data[i] = d;
}

__attribute__ ((noinline)) void 
ggSpectrum_Set20(float * data, float d) 
{
   int i;

   for (i = 0; i < 20; i++)
      data[i] = d;
}

/* { dg-final { scan-tree-dump-times "vectorization not profitable" 1 "vect" { target { ! vect_hw_misalign } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! vect_hw_misalign } } } } */

