/* { dg-do compile } */
/* { dg-require-effective-target arm_dsp } */
/* { dg-options "-O1 -fexpensive-optimizations" } */

int mac(const short *a, const short *b, int sqr, int *sum)
{
  int i;
  int dotp = *sum;

  for (i = 0; i < 150; i++) {
    dotp -= b[i] * a[i];
    sqr -= b[i] * b[i];
  }

  *sum = dotp;
  return sqr;
}

/* { dg-final { scan-assembler-times "smulbb" 2 } } */
