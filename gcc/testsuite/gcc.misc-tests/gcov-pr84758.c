/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int x, y;

static void
foo (int a, int b)
{
  {
    if (a == 1 || a == 2)  /* count(1) */
      {
	x = 4;  /* count(1) */
	if (b == 3)  /* count(1) */
	  x = 6;  /* count(1) */
      }
    else
      x = 15;  /* count(#####) */
  }
}

int
main (void)
{
  foo (2, 3);
  return 0;
}

/* { dg-final { run-gcov gcov-pr84758.c } } */
