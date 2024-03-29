/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

/* If only --exclude is used, other functions should be used by default.  */

int
fn1 (int x)
{
  /* fst */
  return x;
}

int
fn2 (int x)
{
  /* snd */
  return x * 2;
}

int
main ()
{}

/* { dg-final { run-gcov { filters { fst snd } { main } } { --exclude=main gcov-27.c } } } */
