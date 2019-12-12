/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse2 -O2 -ftree-vectorize" } */

#define N 8

int abs (int);

unsigned char pix1[N], pix2[N];

int foo (void)
{
  int i_sum = 0;
  int i;

  for (i = 0; i < N; i++)
    i_sum += abs (pix1[i] - pix2[i]);

  return i_sum;
}

/* { dg-final { scan-assembler "psadbw" } } */
