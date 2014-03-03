/* { dg-do compile } */
/* { dg-options "-O2 -fcilkplus" } */

#define vl(n) vectorlength(2*n)
void
foo (int *a, int *b, int *c)
{
  int i;
#pragma simd vl(4)
  for (i = 0; i < 64; i++)
    a[i] = b[i] * c[i];
}
