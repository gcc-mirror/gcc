/* { dg-options "-O2 -fno-tree-loop-distribute-patterns" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include "ldp_stp_14.h"

/*
** const_2_double_0:
**	stp	xzr, xzr, \[x0\]
**	ret
*/
CONST_FN (2, double, 0);

/* No preference between vectorizing or not vectorizing here.  */
CONST_FN (4, double, 0);

/*
** const_8_double_0:
**	movi	v([0-9]+)\.\d+[bhsd], .*
**	stp	q\1, q\1, \[x0\]
**	stp	q\1, q\1, \[x0, #?32\]
**	ret
*/
CONST_FN (8, double, 0);

/*
** dup_2_double:
**	stp	d0, d0, \[x0\]
**	ret
*/
DUP_FN (2, double);

/*
** dup_4_double:
**	stp	d0, d0, \[x0\]
**	stp	d0, d0, \[x0, #?16\]
**	ret
*/
DUP_FN (4, double);

/*
** dup_8_double:
**	dup	v([0-9])\.2d, v0\.d\[0\]
**	stp	q\1, q\1, \[x0\]
**	stp	q\1, q\1, \[x0, #?32\]
**	ret
*/
DUP_FN (8, double);

/*
** dup_16_double:
**	dup	v([0-9])\.2d, v0\.d\[0\]
**	stp	q\1, q\1, \[x0\]
**	stp	q\1, q\1, \[x0, #?32\]
**	stp	q\1, q\1, \[x0, #?64\]
**	stp	q\1, q\1, \[x0, #?96\]
**	ret
*/
DUP_FN (16, double);

/*
** cons2_1_double:
**	stp	d0, d1, \[x0\]
**	ret
*/
CONS2_FN (1, double);

/*
** cons2_2_double:
**	stp	d0, d1, \[x0\]
**	stp	d0, d1, \[x0, #?16\]
**	ret
*/
CONS2_FN (2, double);

/*
** cons2_4_double:
**	...
**	stp	q[0-9]+, .*
**	ret
*/
CONS2_FN (4, double);

/*
** cons2_8_double:
**	...
**	stp	q[0-9]+, .*
**	ret
*/
CONS2_FN (8, double);

/*
** cons4_1_double:
**	stp	d0, d1, \[x0\]
**	stp	d2, d3, \[x0, #?16\]
**	ret
*/
CONS4_FN (1, double);

/*
** cons4_2_double:
**	stp	d0, d1, \[x0\]
**	stp	d2, d3, \[x0, #?16\]
**	stp	d0, d1, \[x0, #?32\]
**	stp	d2, d3, \[x0, #?48\]
**	ret
*/
CONS4_FN (2, double);

/*
** cons2_8_double:
**	...
**	stp	q[0-9]+, .*
**	ret
*/
CONS4_FN (4, double);

/*
** cons2_8_double:
**	...
**	stp	q[0-9]+, .*
**	ret
*/
CONS4_FN (8, double);
