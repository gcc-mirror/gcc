/* PR tree-optimization/114425 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 2000
_BitInt(8) a;
_BitInt(300) b;
_BitInt(2000) c;

__attribute__((noipa)) unsigned
foo (_BitInt(2000) d)
{
  int o = __builtin_add_overflow_p (d, 0, b);
  _BitInt(2000) m = c * a;
  unsigned u = m;
  return u + o;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 2000
  if (foo (0xfa7ac16f2613255eeb217e871c1f02221e26ce11f82d6a33206ec0ad5d4414722019933c0e2wb) != 1)
    __builtin_abort ();
#endif
}
