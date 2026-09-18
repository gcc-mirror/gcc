/* PR middle-end/127378 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 768
unsigned _BitInt(512) a;
unsigned _BitInt(768) b = -1;

__attribute__((noipa)) void
foo (unsigned _BitInt(135) *p)
{
  a = (_BitInt(135)) (p[0] + p[1]);
}

__attribute__((noipa)) void
bar (unsigned _BitInt(200) *p)
{
  b = p[0] + p[1];
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 768
  static unsigned _BitInt(135) p[2] = { -1, 0 };
  static unsigned _BitInt(200) q[2] = { 1, 2 };
  foo (p);
  if (a != (unsigned _BitInt(512)) -1)
    __builtin_abort ();
  bar (q);
  if (b != 3)
    __builtin_abort ();
#endif
}
