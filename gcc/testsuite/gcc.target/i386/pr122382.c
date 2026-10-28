/* PR target/122382 */
/* { dg-do compile } */
/* { dg-options "-O2 -mf16c -mno-avx512f -fdump-tree-vect-details" } */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 2 "vect" } } */

void
foo (_Float16 *__restrict h, const float *__restrict f)
{
  for (int n = 0; n < 128; ++n)
    h[n] = f[n];
}

/* { dg-final { scan-assembler "vcvtps2ph" } } */
/* { dg-final { scan-assembler-not "vpinsrw" } } */

void
bar (float *__restrict f, const _Float16 *__restrict h)
{
  for (int n = 0; n < 128; ++n)
    f[n] = h[n];
}

/* { dg-final { scan-assembler "vcvtph2ps" } } */
/* { dg-final { scan-assembler-not "vpextrw" } } */
