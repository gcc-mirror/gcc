/* PR middle-end/128699 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 127
_BitInt(127)
foo (_BitInt(127) a)
{
  return -a;
}

int
bar (int t)
{
  return (_BitInt(96)) (unsigned _BitInt(65)) foo (t) / 2;
}
#endif

#if __BITINT_MAXWIDTH__ >= 255
_BitInt(255)
baz (_BitInt(255) a)
{
  return -a;
}

int
qux (int t)
{
  return (_BitInt(192)) (unsigned _BitInt(129)) baz (t) / 2;
}
#endif
