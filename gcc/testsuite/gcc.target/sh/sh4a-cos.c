/* Verify that we generate single-precision sine and cosine approximate
   (fsca) in fast math mode.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O -ffast-math" } */
/* { dg-final { scan-assembler "\tfsca\t" } } */

#if defined __SH4A__ && ! defined __SH4_NOFPU__
#include <math.h>

double test(double f) { return cos(f); }
#else
asm ("fsca\t");
#endif
