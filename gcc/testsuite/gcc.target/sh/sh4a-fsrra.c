/* Verify that we generate single-precision square root reciprocal
   approximate (fsrra) in fast math mode on SH4A with FPU.  */
/* { dg-do compile { target { has_fsrra } } }  */
/* { dg-options "-O -ffast-math" } */
/* { dg-final { scan-assembler "fsrra" } } */

#include <math.h>

float
test (float f)
{
  return 1 / sqrtf (f);
}
