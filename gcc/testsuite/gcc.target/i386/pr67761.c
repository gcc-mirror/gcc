/* PR target/pr67761 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=slm -mno-stackrealign -g" } */
/* { dg-final { scan-assembler "paddq" } } */

void
test (long long *values, long long val, long long delta)
{
  unsigned i;

  for (i = 0; i < 128; i++, val += delta)
    values[i] = val;
}
