/* PR target/pr65105 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=slm -msse4.2 -mno-stackrealign" } */
/* { dg-final { scan-assembler "pand" } } */
/* { dg-final { scan-assembler "por" } } */
/* { dg-final { scan-assembler "ptest" } } */

long long i1, i2, i3, res;

void
test ()
{
  res = i1 | i2;
  if (res)
    res &= i3;
}
