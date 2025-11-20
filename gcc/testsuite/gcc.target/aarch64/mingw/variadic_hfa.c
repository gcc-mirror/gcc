/* { dg-do compile } */
/* { dg-additional-options "-std=c99" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdarg.h>

typedef struct {
  double x;
  double y;
} point2d;

point2d accumulate(int count, ...) {
    int i;
    va_list ap;
    va_start(ap, count);

    point2d acc = {0.0, 0.0};
    for (i = 0; i < count; ++i) {
        point2d v = va_arg(ap, point2d);
        acc.x += v.x;
        acc.y += v.y;
    }

    va_end(ap);
    return acc;
}

/**
 * For aarch64-w64-mingw32 target, the Homogeneous Floating-point Aggregate
 * (HFA) types are not treated specially.
 *
 * This is in contrast to to aarch64-linux-gnu target where double float args
 * would be loaded into 64 bit D registers.
 */

/*
** main:
**	...
**	fmov	d\d+, 2.0e\+0
**	str	d\d+, \[sp, \d+\]
**	fmov	d\d+, 1.0e\+0
**	str	d\d+, \[sp, \d+\]
**	fmov	d\d+, 4.0e\+0
**	str	d\d+, \[sp, \d+\]
**	fmov	d\d+, 3.0e\+0
**	str	d\d+, \[sp, \d+\]
**	fmov	d\d+, 6.0e\+0
**	str	d\d+, \[sp, \d+\]
**	fmov	d\d+, 5.0e\+0
**	str	d\d+, \[sp, \d+\]
**	fmov	d\d+, 8.0e\+0
**	str	d\d+, \[sp, \d+\]
**	fmov	d\d+, 7.0e\+0
**	str	d\d+, \[sp, \d+\]
**	ldp	x\d+, x\d+, \[sp, \d+\]
**	ldp	x\d+, x\d+, \[sp, \d+\]
**	ldp	x\d+, x\d+, \[sp, \d+\]
**	ldp	x\d+, x\d+, \[sp, \d+\]
**  ...
*/
int main()
{
  point2d p1 = {2.0, 1.0};
  point2d p2 = {4.0, 3.0};
  point2d p3 = {6.0, 5.0};
  point2d p4 = {8.0, 7.0};

  accumulate (4, p1, p2, p3, p4);

  return 0;
}
