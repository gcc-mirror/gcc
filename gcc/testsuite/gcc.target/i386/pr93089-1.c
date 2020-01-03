/* PR target/93089 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512f -mtune=skylake-avx512" } */
/* { dg-final { scan-assembler "vmulps\[^\n\r]*zmm" } } */
/* { dg-final { scan-assembler-not "vaddps\[^\n\r]*zmm" } } */
/* { dg-final { scan-assembler "vaddps\[^\n\r]*ymm" } } */

float a[16], b[16];

__attribute__((target ("prefer-vector-width=512"))) void
foo (void)
{
  int i;
  for (i = 0; i < 16; ++i)
    b[i] = 3.0f * a[i];
}

void
bar (void)
{
  int i;
  for (i = 0; i < 16; ++i)
    b[i] = 3.0f + a[i];
}
