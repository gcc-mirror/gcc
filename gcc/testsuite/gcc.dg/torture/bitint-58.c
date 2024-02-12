/* PR tree-optimization/113849 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

signed char c;
unsigned _BitInt(512) b;

__attribute__((noipa)) void
foo (unsigned _BitInt(511) a, int *x)
{
  int z = (a << 510) <= b;
  *x = z + c;
}

int
main ()
{
  int x;
  foo (2, &x);
  if (x != 1)
    __builtin_abort ();
  return 0;
}
