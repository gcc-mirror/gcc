/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13" } */
/* { dg-require-effective-target s390_vx } */

int
foo (int * restrict a, int n)
{
  int i, result = 0;

  for (i = 0; i < n * 4; i++)
    result += a[i];
  return result;
}

/* We do NOT want this loop to get peeled.  Without peeling no scalar
   memory add should appear.  */
/* { dg-final { scan-assembler-not "\ta\t" } } */
