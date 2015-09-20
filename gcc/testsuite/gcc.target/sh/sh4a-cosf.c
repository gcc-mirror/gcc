/* Verify that we generate single-precision sine and cosine approximate
   (fsca) in fast math mode on SH4A with FPU.  */
/* { dg-do compile { target { sh4a && any_fpu } } }  */
/* { dg-options "-O -ffast-math" } */
/* { dg-final { scan-assembler "fsca" } } */

#include <math.h>

float
test (float f)
{
  return cosf (f);
}
