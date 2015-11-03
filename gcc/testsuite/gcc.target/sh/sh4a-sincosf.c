/* Verify that we generate a single single-precision sine and cosine
   approximate (fsca) in fast math mode when a function computes both
   sine and cosine.  */
/* { dg-do compile { target { sh4a && any_fpu } } }  */
/* { dg-options "-O -ffast-math" } */
/* { dg-final { scan-assembler-times "fsca" 1 } } */

#include <math.h>

float
test (float f)
{
  return sinf (f) + cosf (f);
}
