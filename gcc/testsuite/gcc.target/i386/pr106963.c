/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mno-avx2" } */

void
foo_neg_const (int *a)
{
  int i, b = 1;

  for (i = 0; i < 1000; i++)
    {
      a[i] = b;
      b = -b;
    }
}
