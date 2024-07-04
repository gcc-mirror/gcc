/* PR tree-optimization/113120 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

_BitInt(8) a;
_BitInt(55) b;

#if __BITINT_MAXWIDTH__ >= 401
static __attribute__((noinline, noclone)) void
foo (unsigned _BitInt(1) c, _BitInt(401) d)
{
  c /= d << b;
  a = c;
}

void
bar (void)
{
  foo (1, 4);
}
#endif

#if __BITINT_MAXWIDTH__ >= 6928
_BitInt(6928)
baz (int x, _BitInt(6928) y)
{
  if (x)
    return y;
  else
    return 0;
}
#endif
