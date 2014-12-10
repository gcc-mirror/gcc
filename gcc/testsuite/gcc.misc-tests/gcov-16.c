/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

void
bar (void)
{}

void
foo (int i)
{
  if (i > 1)  /* count(1) */
    return;   /* count(#####) */

  bar ();      /* count(1) */
}

int
main (void)
{
  foo (0);
  return 0;
}

/* { dg-final { run-gcov gcov-16.c } } */
