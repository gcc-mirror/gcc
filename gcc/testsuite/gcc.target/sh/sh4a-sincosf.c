/* Verify that we generate a single single-precision sine and cosine
   approximate (fsca) in fast math mode when a function computes both
   sine and cosine.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O -ffast-math" } */
/* { dg-final { scan-assembler-times "\tfsca\t" 1 } } */

#if defined __SH4A__ && ! defined __SH4_NOFPU__
#include <math.h>

float test(float f) { return sinf(f) + cosf(f); }
#else
asm ("fsca\t");
#endif
