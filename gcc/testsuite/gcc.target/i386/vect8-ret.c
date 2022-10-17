/* { dg-do compile { target { ia32 && { ! *-*-vxworks* } } } } */
/* { dg-options "-mmmx -mno-sse -mvect8-ret-in-mem" } */

#include <mmintrin.h>

__m64
vecret (__m64 vect)
{
  return vect;
}

/* { dg-final { scan-assembler-times "movq" 1 } } */
