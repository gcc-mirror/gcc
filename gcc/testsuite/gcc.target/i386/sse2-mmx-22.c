/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include <mmintrin.h>

void
foo (void)
{
  _mm_empty ();
}

/* { dg-final { scan-assembler-times "emms" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not "emms" { target { ! ia32 } } } } */
