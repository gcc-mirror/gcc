/* PR tree-optimization/113119 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

_BitInt(8) b;
_Bool c;
#if __BITINT_MAXWIDTH__ >= 8445
_BitInt(8445) a;

void
foo (_BitInt(4058) d)
{
  c = __builtin_add_overflow (a, 0ULL, &d);
  __builtin_add_overflow (a, 0ULL, &d);
  b = d;
}
#endif
