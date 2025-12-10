/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-max-lmul=conv-dynamic" } */

void foo2x1 (short *restrict a, char *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo2x2 (int *restrict a, short *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo2x3 (long *restrict a, int *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo4x1 (int *restrict a, char *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo4x2 (long *restrict a, short *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

void foo8x (long *restrict a, char *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i];
}

/* { dg-final { scan-assembler-times ",m2," 3 } } */
/* { dg-final { scan-assembler-times ",m4," 2 } } */
/* { dg-final { scan-assembler-times ",m8," 1 } } */
