/* { dg-do compile } */
/* { dg-additional-options "-march=skylake-avx512" { target x86_64-*-* i?86-*-* } } */

int
foo (int *a, double *b, int *c)
{
  int f, g = 0;
  for (f = 0; f < 100; f++)
    if (b[f] && c[a[f]])
      g++;
  return g;
}
