__attribute__ ((noinline)) void
calculate (const double *__restrict__ A, const double *__restrict__ B,
	   double *__restrict__ C)
{
  TYPE m = 0;
  TYPE n = 0;
  TYPE k = 0;

  A = (const double *) __builtin_assume_aligned (A, 16);
  B = (const double *) __builtin_assume_aligned (B, 16);
  C = (double *) __builtin_assume_aligned (C, 16);

  for (n = 0; n < 9; n++)
    {
      for (m = 0; m < 10; m++)
	{
	  C[(n * 10) + m] = 0.0;
	}

      for (k = 0; k < 17; k++)
	{
#pragma simd
	  for (m = 0; m < 10; m++)
	    {
	      C[(n * 10) + m] += A[(k * 20) + m] * B[(n * 20) + k];
	    }
	}
    }
}

