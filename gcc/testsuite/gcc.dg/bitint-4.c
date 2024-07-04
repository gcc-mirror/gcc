/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */

_BitInt(2)
foo (_BitInt(2) x, _BitInt(15) y)
{
  return x + y;
}

_BitInt(64)
bar (_BitInt(64) x, _BitInt(64) y)
{
  return x + y;
}

#if __BITINT_MAXWIDTH__ >= 128
_BitInt(128) a, b, c;

_BitInt(128)
baz (_BitInt(128) x, _BitInt(128) y)
{
  a = x;
  b = y;
  return c;
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
_BitInt(575) d, e, f;

_BitInt(575)
qux (_BitInt(575) x, _BitInt(575) y)
{
  d = x;
  e = y;
  return f;
}
#endif
