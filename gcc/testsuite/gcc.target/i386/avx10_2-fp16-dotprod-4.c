/* { dg-do run } */
/* { dg-options "-O3 -ffast-math -mavx10.2 -save-temps" } */
/* { dg-require-effective-target avx10_2 } */

/* Check the value computed by the vectorized fp16 dot product.  The inputs
   are small integers scaled by a power of two, so every product and every
   partial sum is exact in float and the result does not depend on the
   summation order.  */

__attribute__ ((noipa)) float
dot_fp16 (const _Float16 *restrict a, const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

int
main (void)
{
  if (!__builtin_cpu_supports ("avx10.2"))
    return 0;

  _Float16 a[257], b[257];
  for (int i = 0; i < 257; ++i)
    {
      a[i] = (i % 31 - 15) * 0.0625f;
      b[i] = (i % 13 - 6) * 0.0625f;
    }

  float expected = 0.0f;
  for (int n = 0; n <= 257; ++n)
    {
      if (dot_fp16 (a, b, n) != expected)
	__builtin_abort ();
      if (n < 257)
	expected += (float) a[n] * (float) b[n];
    }
  return 0;
}

/* { dg-final { scan-assembler {\tvdpphps\t} } } */
