/* PR middle-end/129023 */
/* { dg-do run } */
/* { dg-options "-O2" } */

[[gnu::noipa]] unsigned long
foo (unsigned long x)
{
  return __builtin_clmulg (x, 2UL);
}

[[gnu::noipa]] unsigned
bar (unsigned x, unsigned *y)
{
  return __builtin_clmulg (x, 0x80000000U, y);
}

#if __BITINT_MAXWIDTH__ >= 2
[[gnu::noipa]] unsigned _BitInt(2)
baz (unsigned _BitInt(2) x, unsigned _BitInt(2) *y)
{
  return __builtin_clmulg (2uwb, x, y);
}
#endif

[[gnu::noipa]] unsigned long
qux (unsigned long x)
{
  return __builtin_clmulg (x, 0x18UL);
}

int
main ()
{
  unsigned a;
  if (foo (1) != 2 || foo (0x1345U) != 0x268aU)
    __builtin_abort ();
  if (bar (1, &a) != 0x80000000U || a != 0)
    __builtin_abort ();
#if __BITINT_MAXWIDTH__ >= 2
  unsigned _BitInt(2) b;
  if (baz (1, &b) != 2 || b != 0)
    __builtin_abort ();
#endif
  if (qux (1) != 0x18U || qux (0x1345UL) != 0x1ae78UL)
    __builtin_abort ();
}
