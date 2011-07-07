/* { dg-do compile } */
/* { dg-require-effective-target arm_dsp } */
/* { dg-options "-O2" } */

int mac(const int *a, const int *b, long long sqr, long long *sum)
{
  int i;
  long long dotp = *sum;

  for (i = 0; i < 150; i++) {
    dotp += (long long) b[i] * a[i];
    sqr += (long long) b[i] * b[i];
  }

  *sum = dotp;
  return sqr;
}

/* { dg-final { scan-assembler-times "smlal" 2 } } */
