/* PR tree-optimization/89007 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512bw -mavx512vl -mprefer-vector-width=512" } */

void
avg_floor (short *restrict d, short *restrict a, short *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = (a[i] + b[i]) >> 1;
}

void
avg_ceil (short *restrict d, short *restrict a, short *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = (a[i] + b[i] + 1) >> 1;
}

/* { dg-final { scan-assembler "vpsraw\[ \t\]" } } */
/* { dg-final { scan-assembler-not "vpsrad\[ \t\]" } } */
