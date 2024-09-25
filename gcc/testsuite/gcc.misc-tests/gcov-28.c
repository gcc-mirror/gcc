/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int
once (int x)
{
  /* fst */
  return x;
}

int
twice (int x)
{
  /* snd */
  return x * 2;
}

int
main ()
{}

/* { dg-final { run-gcov { filters { fst } { snd main } } { --include=once gcov-28.c } } } */
