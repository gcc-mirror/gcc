/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

/* Filters are considered in order with latest-wins, so if a function is
   included and later excluded it should not show up.  */

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

/* { dg-final { run-gcov { filters { snd } { fst main } } { --include=fn --exclude=1 gcov-25.c } } } */
