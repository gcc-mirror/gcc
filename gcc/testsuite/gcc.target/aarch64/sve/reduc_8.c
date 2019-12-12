/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

int
reduc (int *restrict a, int *restrict b, int *restrict c)
{
  for (int i = 0; i < 100; ++i)
    {
      int res = 0;
      for (int j = 0; j < 100; ++j)
	if (b[i + j] != 0)
	  res = c[i + j];
      a[i] = res;
    }
}

/* { dg-final { scan-assembler-times {\tcmpne\tp[0-9]+\.s, } 1 } } */
/* { dg-final { scan-assembler-not {\tcmpeq\tp[0-9]+\.s, } } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.s, } 1 } } */
