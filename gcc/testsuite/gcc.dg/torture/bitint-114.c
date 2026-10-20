/* PR tree-optimization/127905 */
/* { dg-do run { target bitint } } */

#if __BITINT_MAXWIDTH__ >= 135
typedef unsigned _BitInt(135) B;

[[gnu::noipa]] B
foo (B b, int c, unsigned n)
{
  B x = c ? b : 7;
  B y = b;
  for (unsigned i = 0; i < n; i++)
    y = x + i;
  return y;
}

[[gnu::noipa]] B
bar (B b, B a, int c, unsigned n)
{
  B x = c ? b : 7;
  B y = b;
  for (unsigned i = 0; i < n; i++)
    y = c ? x + 7 : a;
  return y;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 135
  if (foo (5, 1, 3) != 7 || bar (5, 9, 1, 3) != 12)
    __builtin_abort ();
#endif
}
