/* PR tree-optimization/113988 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mavx512f" { target i?86-*-* x86_64-*-* } } */

int i;

#if __BITINT_MAXWIDTH__ >= 256
void
foo (void *p, _BitInt(256) x)
{
  __builtin_memcpy (p, &x, sizeof x);
}

_BitInt(256)
bar (void *p, _BitInt(256) x)
{
  _BitInt(246) y = x + 1;
  __builtin_memcpy (p, &y, sizeof y);
  return x;
}
#endif

#if __BITINT_MAXWIDTH__ >= 512
void
baz (void *p, _BitInt(512) x)
{
  __builtin_memcpy (p, &x, sizeof x);
}

_BitInt(512)
qux (void *p, _BitInt(512) x)
{
  _BitInt(512) y = x + 1;
  __builtin_memcpy (p, &y, sizeof y);
  return x;
}
#endif
