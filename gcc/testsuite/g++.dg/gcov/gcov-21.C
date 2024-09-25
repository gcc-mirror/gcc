/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

/* Filters can be applied to demangled names. This support matching on
   types and class hierarchies as well as function names.  */

template <typename T>
T
fn1 (T x)
{
  /* fst */
  return x;
}

template <typename T>
T
fn2 (T x)
{
  /* snd */
  return 2 * x;
}

int
main ()
{
  fn1 (2);

  fn2 (2.0);
  fn2 (2.0f);
}

/* { dg-final { run-gcov { filters { fst } } { --filter-on-demangled --include int gcov-21.C } } } */
