/* Uninitialized variable warning tests...
   Inspired by part of optabs.c:expand_binop.
   May be the same as uninit-1.c.  */

/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

#include <limits.h>

void
add_bignums (int *out, int *x, int *y)
{
    int p, sum;
    int carry; /* { dg-bogus "carry" "uninitialized variable warning" } */

    p = 0;
    for (; *x; x++, y++, out++, p++)
    {
	if (p)
	    sum = *x + *y + carry;
	else
	    sum = *x + *y;

	if (sum < 0)
	{
	    carry = 1;
	    sum -= INT_MAX;
	}
	else
	    carry = 0;
    }
}
