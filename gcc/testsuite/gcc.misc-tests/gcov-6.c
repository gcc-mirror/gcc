/* Check that call return percentages are reported correctly by gcov,
   along with line counts and branch percentages.  This test case is
   meant to be simple, as it was added at the same time that checking
   for call return percentages was added.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int val;

void
foo (int i)
{
  					/* branch(80) */
  if (i < 0)				/* count(5) */
    					/* branch(end) */
    					/* returns(0) */
    exit (0);				/* count(1) */
  					/* returns(end) */
  val += i;				/* count(4) */
}

int
main()
{
  int i;

  					/* returns(100) */
  foo (100);				/* count(1) */
  					/* returns(end) */
  for (i = 2; i > -10; i--)
    					/* returns(75) */
    foo (i);				/* count(4) */
  					/* returns(end) */
}

/* { dg-final { run-gcov branches calls { -b gcov-6.c } } } */
