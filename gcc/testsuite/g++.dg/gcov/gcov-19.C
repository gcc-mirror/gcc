/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

/* Filtering on the function base name generally works well, because it becomes
   an unadultered part of the symbol.  */

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
  fn1 (2.0);
  fn1 (2.0f);

  fn2 (2);
  fn2 (2.0);
  fn2 (2.0f);
}

/* { dg-final { run-gcov { filters { fn1 } { fn2 } } { --include=fn1 gcov-19.C } } } */
