/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -Ofast -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times "vcvttph2dq" "2" } } */
/* { dg-final { scan-assembler-times "vcvtdq2ph" "2" } } */
/* { dg-final { scan-assembler-times "vcvtph2ps" "2" } } */
/* { dg-final { scan-assembler-times "vcvtps2ph" "2" } } */

void
foo (int* __restrict a, _Float16* b)
{
  for (int i = 0; i != 1000000; i++)
    a[i] = b[i];
}

void
foo1 (int* __restrict a, _Float16* b)
{
  for (int i = 0; i != 100000; i++)
    b[i] = a[i];
}

void
foo2 (float* __restrict a, _Float16* b)
{
  for (int i = 0; i != 1000000; i++)
    a[i] = b[i];
}

void
foo3 (float* __restrict a, _Float16* b)
{
  for (int i = 0; i != 100000; i++)
    b[i] = a[i];
}
