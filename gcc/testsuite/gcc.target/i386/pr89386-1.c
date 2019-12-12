/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mssse3 -O2 -ftree-vectorize" } */

#define N 4

short a[N], b[N], c[N];

void foo (void)
{
  int i;

  for (i = 0; i < N; i++)
    a[i] = ((((int) b[i] * (int) c[i]) >> 14) + 1) >> 1;
}

/* { dg-final { scan-assembler "pmulhrsw" } } */
