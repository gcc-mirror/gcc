/* PR tree-optimization/113614 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

_BitInt(8) a;
_BitInt(8) b;
_BitInt(8) c;

#if __BITINT_MAXWIDTH__ >= 256
_BitInt(256)
foo (_BitInt(8) y, unsigned _BitInt(256) z)
{
  unsigned _BitInt(256) d = -y;
  z /= d;
  return z + a + b + c;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 256
  if (foo (0xfwb, 0x24euwb))
    __builtin_abort ();
#endif
  return 0;
}
