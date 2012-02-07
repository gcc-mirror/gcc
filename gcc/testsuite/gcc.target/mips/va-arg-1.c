/* See PR 52154 for the xfail.  */
/* { dg-do run { xfail { mips_eabi && { hard_float && ilp32 } } } } */

#include <stdarg.h>

extern void abort (void);

struct __attribute__((aligned(16))) empty {};

static void __attribute__((noinline))
check_args (int count, ...)
{
  va_list va;
  int i;

  va_start (va, count);
  for (i = 0; i < count; i++)
    if (va_arg (va, int) != 1000 + i)
      abort ();

  va_arg (va, struct empty);
  if (va_arg (va, int) != 2000 + count)
    abort ();

  va_end (va);
}

int
main (void)
{
  struct empty e;

  check_args (1, 1000, e, 2001);
  check_args (2, 1000, 1001, e, 2002);
  check_args (3, 1000, 1001, 1002, e, 2003);
  check_args (4, 1000, 1001, 1002, 1003, e, 2004);
  check_args (5, 1000, 1001, 1002, 1003, 1004, e, 2005);
  check_args (6, 1000, 1001, 1002, 1003, 1004, 1005, e, 2006);
  check_args (7, 1000, 1001, 1002, 1003, 1004, 1005, 1006, e, 2007);
  check_args (8, 1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007, e, 2008);
  check_args (9, 1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007,
	      1008, e, 2009);
  check_args (10, 1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007,
	      1008, 1009, e, 2010);
  check_args (11, 1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007,
	      1008, 1009, 1010, e, 2011);
  return 0;
}
