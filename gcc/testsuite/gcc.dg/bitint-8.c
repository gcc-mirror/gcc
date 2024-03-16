/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23 -Wno-uninitialized" } */

#if __BITINT_MAXWIDTH__ >= 135
_BitInt(135)
foo (void)
{
  _BitInt(135) d;
  _BitInt(135) e = d + 2wb;
  return e;
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
_BitInt(575)
bar (void)
{
  _BitInt(575) d;
  _BitInt(575) e = d * 42wb;
  return e;
}

_BitInt(575)
baz (int x)
{
  _BitInt(575) d;
  if (x)
    return 59843758943759843574wb;
  return d;
}
#endif

int x;
