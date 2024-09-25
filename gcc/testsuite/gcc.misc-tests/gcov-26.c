/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

/* Filters are considered in order with latest-wins, so if a function is
   excluded and later included it should show up.  */

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

/* { dg-final { run-gcov { filters { fst snd } { main } } { --exclude=1 --include=fn gcov-26.c } } } */
