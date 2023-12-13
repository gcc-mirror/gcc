/* PR tree-optimization/112940 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 1025
_BitInt (1025) b;
#endif

void
foo (long x)
{
#if __BITINT_MAXWIDTH__ >= 1025
  b += (unsigned _BitInt (255)) x;
#else
  (void) x;
#endif
}
