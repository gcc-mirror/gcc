/* Check that gcov correctly reports line counts, branch percentages,
 * and call return percentages for functions that call longjmp. */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

#include <setjmp.h>

extern void abort (void);
extern void exit (int);

jmp_buf env;
int val;
int longjmp_taken;
int bar_enter, bar_exit;
int foo_enter, foo_exit;

void bar (int i)
{
  bar_enter++;				/* count(3) */
					/* branch(67) */
  if (i == 0) {
    					/* branch(end) */
      longjmp_taken++;			/* count(1) */
      longjmp (env, 1);
    }
  val += i+1;
  bar_exit++;				/* count(2) */
}

void foo (int i)
{
  foo_enter++;				/* count(3) */
					/* branch(67) */
  if (i == 1) {
					/* branch(end) */
      longjmp_taken++;			/* count(1) */
      longjmp (env, 2);
    }
					/* returns(50) */
  bar (i);				/* count(2) */
					/* returns(100) */
  bar (7);				/* count(1) */
					/* returns(end) */
  val += 16;
  foo_exit++;				/* count(1) */
}

int
passed ()
{
  return (val == 31 &&
          longjmp_taken == 2 &&
	  foo_enter == 3 &&
	  foo_exit == 1 &&
	  bar_enter == 3 &&
	  bar_exit == 2);
	
}

void
leave (int i)
{
  if (i == 0) {
      abort ();
    }
  exit (0);
}

int
main()
{
  int retval;

					/* branch(33) */
  if ((retval = setjmp (env))) {
					/* branch(end) */
      val += retval;			/* count(2) */
    }
					/* returns(33) */
  foo (val);				/* count(3) */
					/* returns(0) */
  leave (passed());			/* count(1) */
					/* returns(end) */
}

/* { dg-final { run-gcov calls branches { -b gcov-7.c } } } */
