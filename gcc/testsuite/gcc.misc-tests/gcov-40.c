/* { dg-options "--coverage" } */
/* { dg-do run } */
/* { dg-require-effective-target indirect_jumps } */

#include <setjmp.h>
extern void abort (void);
extern void exit (int);

jmp_buf longjmp1_env;
int longjmp1_val;
int longjmp1_taken;
int longjmp1_bar_enter, longjmp1_bar_exit;
int longjmp1_foo_enter, longjmp1_foo_exit;

/* Based on gcov-7.c  */

void
longjmp1_bar (int i)
{
  longjmp1_bar_enter++;		/* count(3) */
  if (i == 0) {
#pragma GCC suppress_coverage begin
    longjmp1_taken++;			/* count(#) */
#pragma GCC suppress_coverage end
    longjmp (longjmp1_env, 1);		/* count(1) */
  }
  longjmp1_val += i+1;			/* count(2) */
  longjmp1_bar_exit++;			/* count(2) */
}

void
longjmp1_foo (int i)
{
  longjmp1_foo_enter++;		/* count(3) */
  if (i == 1) {
    longjmp1_taken++;			/* count(1) */
#pragma GCC suppress_coverage begin
    longjmp (longjmp1_env, 2);		/* count(#) */
#pragma GCC suppress_coverage end
  }
  longjmp1_bar (i);			/* count(2) */
  longjmp1_bar (7);			/* count(1) */
  longjmp1_val += 16;
  longjmp1_foo_exit++;			/* count(1) */
}

void
longjmp1 ()
{
  int retlongjmp1_val;
#pragma GCC suppress_coverage begin
  if ((retlongjmp1_val = setjmp (longjmp1_env))) {
    longjmp1_val += retlongjmp1_val;		/* count(#) */
  }
#pragma GCC suppress_coverage end
  longjmp1_foo (longjmp1_val);			/* count(3) */

  if (!(longjmp1_val == 31 &&
	longjmp1_taken == 2 &&
	longjmp1_foo_enter == 3 &&
	longjmp1_foo_exit == 1 &&
	longjmp1_bar_enter == 3 &&
	longjmp1_bar_exit == 2))
    abort ();
}

/* Based on pr85372.c  */
void *buf[5];

void fjmp (void) {
  __builtin_longjmp (buf, 1);
}

int
pr85372 (void)
{
  int last = 0;

  if (__builtin_setjmp (buf) == 0) {	/* count(2) */
    __builtin_printf("True  branch\n");
#pragma GCC suppress_coverage begin
    while (1) {
      last = 1;			/* count(#) */
      fjmp ();				/* count(#) */
    }
#pragma GCC suppress_coverage end
  } else {
    __builtin_printf("False branch\n");
  }

  return 0;
}

int main ()
{
  longjmp1 ();
  pr85372 ();
}

/* { dg-final { run-gcov { gcov-40.c } } } */
