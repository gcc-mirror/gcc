/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "fmsub\[ \ta-zA-Z0-9\]*," 1 } } */

float
f (float ar, float ai, float br, float bi)
{
  return ar * br - ai * bi;
}
