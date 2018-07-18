/* PR rtl-optimization/79901 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f -fno-ssa-phiopt" } */

unsigned int
foo (const unsigned long long x)
{
  if (x < 0)
    return 0;
  else if ( x > ~0U)
    return ~0U;
  else
    return (unsigned int) x;
}

void
bar (unsigned x, unsigned int *y, unsigned int z)
{
  unsigned i;
  for (i = 0; i < x; i++)
    y[i] = foo (y[i] * (unsigned long long) z);
}
