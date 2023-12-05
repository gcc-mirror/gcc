/* PR tree-optimization/112843 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 256
_BitInt (256)
foo (_BitInt (128) x, _BitInt (256) y)
{
  return x * 5 * y;
}
#else
int x;
#endif
