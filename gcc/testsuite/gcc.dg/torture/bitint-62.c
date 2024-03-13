/* PR middle-end/114313 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 256
struct S { _BitInt(257) : 257; _BitInt(256) b : 182; } s;

__attribute__((noipa)) _BitInt(256)
foo (void)
{
  return s.b;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 256
  s.b = 1414262180967678524960294186228886540125217087586381431wb;
  if (foo () != 1414262180967678524960294186228886540125217087586381431wb)
    __builtin_abort ();
  s.b = -581849792837428541666755934071828568425158644418477999wb;
  if (foo () != -581849792837428541666755934071828568425158644418477999wb)
    __builtin_abort ();
#endif
}
