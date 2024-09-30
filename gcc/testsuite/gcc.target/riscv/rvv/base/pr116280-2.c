/* Test there is no ICE when compile.  */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvbb -mabi=lp64d -O3" } */

void
test (short *restrict dst, char *restrict a, int *restrict b, int n)
{
  for (int i = 0; i < n; i++)
    dst[i] = a[i] << b[i];
}
