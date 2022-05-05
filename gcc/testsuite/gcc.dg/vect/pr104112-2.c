/* { dg-do compile } */
/* Diagnose vector ops that are later decomposed.  */
/* { dg-additional-options "-Wvector-operation-performance" } */

unsigned short foo (unsigned short *a, int n)
{
  unsigned short sum = 0;
  for (int i = 0; i < n; ++i)
    sum += a[i];
  return sum;
}
