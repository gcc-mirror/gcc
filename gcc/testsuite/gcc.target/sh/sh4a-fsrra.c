/* Verify that we generate single-precision square root reciprocal
   approximate (fsrra) in fast math mode on SH4A with FPU.  */
/* { dg-do compile }  */
/* { dg-options "-O -ffast-math" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m4a" "-m4a-single" "-m4a-single-only" } }  */
/* { dg-final { scan-assembler "fsrra" } } */

#include <math.h>

float test(float f) { return 1 / sqrtf(f); }

