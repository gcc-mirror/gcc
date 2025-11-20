/* { dg-do compile } */
/* { dg-additional-options "-std=c99" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdarg.h>

/*
** sum:
**	...
**	str	w0, \[sp, \d+\]
**	str	x1, \[sp, \d+\]
**	str	x2, \[sp, \d+\]
**	str	x3, \[sp, \d+\]
**	str	x4, \[sp, \d+\]
**	str	x5, \[sp, \d+\]
**	str	x6, \[sp, \d+\]
**	str	x7, \[sp, \d+\]
**	add	x0, sp, \d+
**	sub	x0, x0, #\d+
**	str	x0, \[sp, \d+\]
**	str	wzr, \[sp, \d+\]
**	str	wzr, \[sp, \d+\]
**	...
*/
int sum(int count, ...) {
    va_list args;

    va_start(args, count);

    int total = 0;
    for (int i = 0; i < count; i++)
      {
        total += va_arg(args, int);
      }

    va_end(args);

    return total;
}

/* { dg-final { scan-assembler-not "str\tq\[0-9\]+, \[sp, \[0-9\]+\]*" } } */