/* Test for bogus "variable `x' may be clobbered by longjmp" warnings.
   Inspired by cse.c:simplify_relational_operation. */

/* { dg-do compile } */
/* { dg-options "-O -Wclobbered -Wextra -Wall" } */
/* { dg-skip-if "" { ! nonlocal_goto } { "*" } { "" } } */

#include <setjmp.h>

extern void set_float_handler (jmp_buf *);

#define EQ 0x01
#define LT 0x02
#define GT 0x04

int
compare_float (double a, double b)  /* { dg-bogus "clobbered" "spurious clobbered warning" } */
{
  jmp_buf handler;
  int result;

  a += 1.0;

  if (setjmp (handler))
    {
      set_float_handler (0);
      return 0;
    }

  set_float_handler (&handler);
  if (a == b) result = EQ;
  else if (a > b) result = LT;
  else if (a < b) result = GT;
  else result = 0;
  set_float_handler (0);
  return result;
}
