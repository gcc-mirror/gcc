/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

/* Filtering also works by targeting the mangled symbol directly, but the
   subtlety is not really caught by the test framework.  Matching on fn1I[df]
   prints the "overlapping blocks" of both the float and double instantiation,
   but *not* the int instantiation.  The extra {} around the --include argument
   is quoting for tcl/dejagnu.  */

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

/* { dg-final { run-gcov { filters { fst } { snd } } { {--include=fn1I[fd]} gcov-20.C } } } */
