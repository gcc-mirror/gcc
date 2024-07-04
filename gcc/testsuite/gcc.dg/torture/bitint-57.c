/* PR tree-optimization/113774 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 512
unsigned _BitInt(512) u;
unsigned _BitInt(512) v;

void
foo (unsigned _BitInt(255) a, unsigned _BitInt(257) b, unsigned _BitInt(512) *r)
{
  b += v;
  b |= a - b;
  unsigned _BitInt(512) c = b * 6;
  unsigned _BitInt(512) h = c >> u;
  *r = h;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 512
  unsigned _BitInt(512) x;
  foo (0x10000000000000000wb, 0x10000000000000001wb, &x);
  if (x != 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffawb)
    __builtin_abort ();
#endif
  return 0;
}
