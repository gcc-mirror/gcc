/* PR tree-optimization/114041 */
/* { dg-require-effective-target bitint } */
/* { dg-options "-O -fgraphite-identity" } */

unsigned a[24], b[24];

__attribute__((noipa)) unsigned
foo (unsigned _BitInt(8) x)
{
  for (int i = 0; i < 24; ++i)
    a[i] = i;
  unsigned e = __builtin_stdc_bit_ceil (x);
  for (int i = 0; i < 24; ++i)
    b[i] = i;
  return e;
}

int
main ()
{
  if (foo (0) != 1)
    __builtin_abort ();
}
