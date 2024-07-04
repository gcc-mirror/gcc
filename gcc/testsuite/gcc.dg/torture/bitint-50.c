/* PR tree-optimization/113421 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 1024
unsigned _BitInt(1024) a = -5wb;

__attribute__((noipa)) void
foo (unsigned _BitInt(1024) x)
{
  a *= x;
}
#else
int a = 30;

void
foo (int)
{
}
#endif

int
main ()
{
  foo (-6wb);
  if (a != 30wb)
    __builtin_abort ();
  return 0;
}
