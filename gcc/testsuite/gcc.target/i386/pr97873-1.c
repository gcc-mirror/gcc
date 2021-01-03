/* PR target/97873 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mavx512vl -mstv -mno-stackrealign" } */
/* { dg-final { scan-assembler "pabsq" } } */

extern long long z;

void
foo (long long x)
{
  z = (x < 0) ? -x : x;
}
