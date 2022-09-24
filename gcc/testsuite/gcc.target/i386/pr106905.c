/* { dg-do compile } */
/* { dg-options "-march=silvermont -O2 -fvect-cost-model=dynamic" } */

void
foo_mul_peel (int *a, int b)
{
  int i;

  for (i = 0; i < 7; ++i)
    {
      b *= 2;
      a[i] = b;
    }
}
