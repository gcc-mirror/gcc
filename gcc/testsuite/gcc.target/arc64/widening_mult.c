/* { dg-options "-O2 -msimd -ftree-vectorize" } */

void
foo (int N, int* c, short* a, short val)
{
  int i,j;
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
	{
	  c[i * N + j]=(int)a[i * N + j] * (int)val;
	}
    }
}

/* { dg-final { scan-assembler-times "vmpy2h" 2 } } */
