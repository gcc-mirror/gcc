/* PR tree-optimization/113568 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23" } */

signed char c;
#if __BITINT_MAXWIDTH__ >= 464
_BitInt(464) g;

void
foo (void)
{
  _BitInt(464) a[2] = {};
  _BitInt(464) b;
  while (c)
    {
      b = g + 1;
      g = a[0];
      a[0] = b;
    }
}
#endif
