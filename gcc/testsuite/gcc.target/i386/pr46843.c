/* { dg-do compile } */
/* { dg-options "-O2 -fschedule-insns" } */

void foo (double *d1, double *u1, double *u2, double *d2, int s, int j, int i)
{
  int n = 1 << s;
  double x = 0;

  for (; j < n; j++)
    x += d1[j] * d2[i];
  d1[i] = x;
}
