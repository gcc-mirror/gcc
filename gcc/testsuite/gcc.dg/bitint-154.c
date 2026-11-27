/* PR middle-end/128982 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

unsigned _BitInt(32)
foo (unsigned _BitInt(32) a, unsigned _BitInt(32) b)
{
#if __BITINT_MAXWIDTH__ >= 192
  _BitInt(192) s = 0;
  int o = __builtin_add_overflow ((_BitInt(129)) a << 60, (_BitInt(129)) b, &s);
  return (unsigned _BitInt(32)) (s & 0xffffffffuwb) + (unsigned _BitInt(32)) o;
#else
  return a + b;
#endif
}
