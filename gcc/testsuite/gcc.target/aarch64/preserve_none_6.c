/* { dg-do run } */
/* { dg-options "-O2 -std=gnu23" } */

#include <stdarg.h>
#include <stdio.h>

int preserve_none_va_func
  [[gnu::preserve_none, gnu::noinline, gnu::noclone]] (int count, ...)
{
  asm volatile("mov x0, #0;"
	       "mov x1, #0;"
	       "mov x2, #0;"
	       "mov x3, #0;"
	       "mov x4, #0;"
	       "mov x5, #0;"
	       "mov x6, #0;"
	       "mov x7, #0;"
	       "mov x8, #0;"
	       "mov x9, #0;"
	       "mov x10, #0;"
	       "mov x11, #0;"
	       "mov x12, #0;"
	       "mov x13, #0;"
	       "mov x14, #0;"
	       "mov x15, #0;"
	       "mov x16, #0;"
	       "mov x17, #0;"
	       "mov x18, #0;"
	       "mov x19, #0;"
	       "mov x20, #0;"
	       "mov x21, #0;"
	       "mov x22, #0;"
	       "mov x23, #0;"
	       "mov x24, #0;"
	       "mov x25, #0;"
	       "mov x26, #0;"
	       "mov x27, #0;"
	       "mov x28, #0;" ::
		 : "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
		   "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17",
		   "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25",
		   "x26", "x27", "x28");

  int sum = 0;

  va_list args;

  va_start (args, count);
  for (int i = 0; i < count; i++)
    sum += va_arg (args, int);
  va_end (args);

  return sum;
}

int
main ()
{
  int res = preserve_none_va_func (23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
				   13, 14, 15, 16, 17, 18, 19, 20, 21, 22);
  if (res != 23 * 22 / 2)
    return 1;

  res = preserve_none_va_func (24, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
			       14, 15, 16, 17, 18, 19, 20, 21, 22, 23);

  if (res != 24 * 23 / 2)
    return 1;

  res = preserve_none_va_func (25, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
			       14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24);
  if (res != 25 * 24 / 2)
    return 1;

  return 0;
}
