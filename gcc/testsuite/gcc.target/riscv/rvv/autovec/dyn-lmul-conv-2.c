/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-max-lmul=conv-dynamic" } */

void foo2x1 (unsigned char *restrict a, unsigned short *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo2x2 (unsigned short *restrict a, unsigned int *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo2x3 (unsigned int *restrict a, unsigned long *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo4x1 (unsigned char *restrict a, unsigned int *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo4x2 (unsigned short *restrict a, unsigned long *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo8x (unsigned char *restrict a, unsigned long *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

/* { dg-final { scan-assembler-times ",m1," 6 } } */
/* { dg-final { scan-assembler-times ",m2," 3 } } */
/* { dg-final { scan-assembler-times ",m4," 1 } } */
/* { dg-final { scan-assembler-not ",mf2," } } */
