/* PR tree-optimization/112941 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O1 -fno-tree-forwprop" } */

#if __BITINT_MAXWIDTH__ >= 6384
unsigned _BitInt(2049)
foo (unsigned _BitInt(6384) x, _BitInt(8) y)
{
  unsigned _BitInt(6384) z = y;
  return x * z;
}

_BitInt(2049)
bar (unsigned _BitInt(6384) x, _BitInt(1023) y)
{
  unsigned _BitInt(6384) z = y;
  return x * z;
}
#else
int i;
#endif
