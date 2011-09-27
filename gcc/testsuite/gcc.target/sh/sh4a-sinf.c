/* Verify that we generate single-precision sine and cosine approximate
   (fsca) in fast math mode on SH4A with FPU.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O -ffast-math" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m4a" "-m4a-single" "-m4a-single-only" } }  */
/* { dg-final { scan-assembler "fsca" } } */

#include <math.h>

float test(float f) { return sinf(f); }

