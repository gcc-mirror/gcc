/* PR tree-optimization/113783 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mavx512f" { target i?86-*-* x86_64-*-* } } */

int i;

#if __BITINT_MAXWIDTH__ >= 246
void
foo (void *p, _BitInt(246) x)
{
  __builtin_memcpy (p, &x, sizeof x);
}

_BitInt(246)
bar (void *p, _BitInt(246) x)
{
  _BitInt(246) y = x + 1;
  __builtin_memcpy (p, &y, sizeof y);
  return x;
}
#endif

#if __BITINT_MAXWIDTH__ >= 502
void
baz (void *p, _BitInt(502) x)
{
  __builtin_memcpy (p, &x, sizeof x);
}

_BitInt(502)
qux (void *p, _BitInt(502) x)
{
  _BitInt(502) y = x + 1;
  __builtin_memcpy (p, &y, sizeof y);
  return x;
}
#endif
