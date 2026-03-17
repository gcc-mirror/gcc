/* { dg-do compile } */
/* { dg-additional-options "-fno-trapping-math -mcpu=neoverse-v2" { target aarch64-*-* } } */

void foo(int *__restrict a, int pblh, int *__restrict res, int n)
{
  int wts = a[0];
  for(int i = 0; i < n;i++)
  {
    if (a[i] < pblh && wts > 0)
      res[i] = a[i];
  }
}
