/* Verify that we generate single-precision square root reciprocal
   approximate (fsrra) in fast math mode.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O -ffast-math" } */
/* { dg-final { scan-assembler "\tfsrra\t" } } */

#if defined __SH4A__ && ! defined __SH4_NOFPU__
#include <math.h>

float test(float f) { return 1 / sqrtf(f); }
#else
asm ("fsrra\t");
#endif
