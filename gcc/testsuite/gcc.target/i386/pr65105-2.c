/* PR target/pr65105 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -msse2" } */
/* { dg-final { scan-assembler "por" } } */

long long i1, i2, res;

void
test ()
{
  res = i1 | i2;
}
