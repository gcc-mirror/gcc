/* { dg-do compile { target ia32 } } */
/* { dg-options "-mmmx" { target i?86-*-solaris2.[89] *-*-vxworks* } } */
/* { dg-options "-mmmx -mvect8-ret-in-mem" } */

#include <mmintrin.h>

__m64
vecret (__m64 vect)
{
  return vect;
}

/* { dg-final { scan-assembler-times "movq" 1 } } */
