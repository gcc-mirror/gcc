/* Verify that we generate a single single-precision sine and cosine
   approximate (fsca) in fast math mode when a function computes both
   sine and cosine.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O -ffast-math" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m4a" "-m4a-single" "-m4a-single-only" } }  */
/* { dg-final { scan-assembler-times "fsca" 1 } } */

#include <math.h>

float test(float f) { return sinf(f) + cosf(f); }

