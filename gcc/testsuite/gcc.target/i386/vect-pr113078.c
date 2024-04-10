/* { dg-do compile } */
/* { dg-options "-O3 -mavx512vl" } */

int
foo (int n, int* p, int* pi)
{
  int sum = 0;
  for (int i = 0; i != n; i++)
    {
      if (pi[i] > 0)
	sum -= p[i];
    }
  return sum;
}

/* { dg-final { scan-assembler-times "vpsub\[^\r\n\]*%k" 2 } } */
