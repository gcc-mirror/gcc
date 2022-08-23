/* PR tree-optimization/104389 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target inf } */

__attribute__((noipa)) double
foo (void)
{
  double a = __builtin_huge_val ();
  return a * 0.0;
}

__attribute__((noipa)) long double
bar (void)
{
  return __builtin_huge_vall () * 0.0L;
}

int
main ()
{
  if (!__builtin_isnan (foo ()) || !__builtin_isnanl (bar ()))
    __builtin_abort ();
  return 0;
}
