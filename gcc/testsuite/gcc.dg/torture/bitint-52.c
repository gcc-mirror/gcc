/* PR middle-end/113574 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

unsigned _BitInt(1) a;
unsigned _BitInt(8) b;

void
foo (unsigned _BitInt(16) x)
{
  a += (x << 2) | b;
}

int
main ()
{
  foo (0xfef1uwb);
  if (a)
    __builtin_abort ();
  return 0;
}
