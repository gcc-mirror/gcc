/* PR middle-end/121828 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

void baz (int);
#if __BITINT_MAXWIDTH__ >= 255
unsigned _BitInt(255) a;

void
foo (int x, int y)
{
  unsigned _BitInt(255) b;
  int t = __builtin_sub_overflow (y, x, &b);
  baz (t);
  a = b;
}

void
bar (int x, int y)
{
  unsigned _BitInt(255) b;
  bool t = __builtin_sub_overflow (y, x, &b);
  a = b;
  baz (t);
}
#endif
