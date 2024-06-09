/* PR tree-optimization/114433 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 511
struct S { int : 31; _BitInt(511) b : 300; } s;

__attribute__((noipa)) _BitInt(511)
foo (void) 
{
  return s.b << 1;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 511
  s.b = 642460398785925402356009598661384732715767737595497615767135001949421105426024498988100867wb;
  if (foo () != ((_BitInt(511)) 642460398785925402356009598661384732715767737595497615767135001949421105426024498988100867wb) << 1)
    __builtin_abort ();
  s.b = 2655156766298562299560755420298083843774074962786295887660222690220887wb;
  if (foo () != ((_BitInt(511)) 2655156766298562299560755420298083843774074962786295887660222690220887wb) << 1)
    __builtin_abort ();
#endif
}
