__attribute__ ((noipa)) float
dot_half (const _Float16 *restrict a, const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

int
main (void)
{
  _Float16 a[257], b[257];
  for (int small = 0; small < 2; ++small)
    {
      /* Both normal and subnormal inputs, with exact FP32 products and
	 sums.  The small products would underflow if evaluated in FP16.  */
      float scale = small ? 0x1p-24f : 0.0625f;
      for (int i = 0; i < 257; ++i)
	{
	  a[i] = (i % 31 - 15) * scale;
	  b[i] = (i % 13 - 6) * scale;
	}

      double expected = 0.0;
      for (int n = 0; n <= 257; ++n)
	{
	  if (dot_half (a, b, n) != (float) expected)
	    __builtin_abort ();
	  if (n < 257)
	    expected += (double) a[n] * (double) b[n];
	}
    }
  return 0;
}

