/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target ilp32 } */

long long mac(const int *a, const int *b, long long sqr, long long *sum)
{
  int i;
  long long dotp = *sum;

  for (i = 0; i < 150; i++) {
    dotp += (long long)b[i] * a[i];
    sqr += (long long)b[i] * b[i];
  }

  *sum = dotp;
  return sqr;
}

/* { dg-final { scan-assembler-times "imull" 2 } } */
