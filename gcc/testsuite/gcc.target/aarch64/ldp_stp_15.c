/* { dg-options "-O2 -fno-tree-loop-distribute-patterns" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include "ldp_stp_14.h"

/*
** const_2_int32_t_0:
**	str	xzr, \[x0\]
**	ret
*/
CONST_FN (2, int32_t, 0);

/*
** const_4_int32_t_0:
**	stp	xzr, xzr, \[x0\]
**	ret
*/
CONST_FN (4, int32_t, 0);

/* No preference between vectorizing or not vectorizing here.  */
CONST_FN (8, int32_t, 0);

/*
** const_16_int32_t_0:
**	movi	v([0-9]+)\.4s, .*
**	stp	q\1, q\1, \[x0\]
**	stp	q\1, q\1, \[x0, #?32\]
**	ret
*/
CONST_FN (16, int32_t, 0);

/* No preference between vectorizing or not vectorizing here.  */
CONST_FN (2, int32_t, 1);

/*
** const_4_int32_t_1:
**	movi	v([0-9]+)\.4s, .*
**	str	q\1, \[x0\]
**	ret
*/
CONST_FN (4, int32_t, 1);

/*
** const_8_int32_t_1:
**	movi	v([0-9]+)\.4s, .*
**	stp	q\1, q\1, \[x0\]
**	ret
*/
CONST_FN (8, int32_t, 1);

/*
** dup_2_int32_t:
**	stp	w1, w1, \[x0\]
**	ret
*/
DUP_FN (2, int32_t);

/*
** dup_4_int32_t:
**	stp	w1, w1, \[x0\]
**	stp	w1, w1, \[x0, #?8\]
**	ret
*/
DUP_FN (4, int32_t);

/*
** dup_8_int32_t:
**	dup	v([0-9]+)\.4s, w1
**	stp	q\1, q\1, \[x0\]
**	ret
*/
DUP_FN (8, int32_t);

/*
** cons2_1_int32_t:
**	stp	w1, w2, \[x0\]
**	ret
*/
CONS2_FN (1, int32_t);

/*
** cons2_2_int32_t:
**	stp	w1, w2, \[x0\]
**	stp	w1, w2, \[x0, #?8\]
**	ret
*/
CONS2_FN (2, int32_t);

/*
** cons2_4_int32_t:
**	stp	w1, w2, \[x0\]
**	stp	w1, w2, \[x0, #?8\]
**	stp	w1, w2, \[x0, #?16\]
**	stp	w1, w2, \[x0, #?24\]
**	ret
*/
CONS2_FN (4, int32_t);

/* No preference between vectorizing or not vectorizing here.  */
CONS2_FN (8, int32_t);

/*
** cons2_16_int32_t:
**	...
**	stp	q[0-9]+, .*
**	ret
*/
CONS2_FN (16, int32_t);

/*
** cons4_1_int32_t:
**	stp	w1, w2, \[x0\]
**	stp	w3, w4, \[x0, #?8\]
**	ret
*/
CONS4_FN (1, int32_t);

/*
** cons4_2_int32_t:
**	stp	w1, w2, \[x0\]
**	stp	w3, w4, \[x0, #?8\]
**	stp	w1, w2, \[x0, #?16\]
**	stp	w3, w4, \[x0, #?24\]
**	ret
*/
CONS4_FN (2, int32_t);

/* No preference between vectorizing or not vectorizing here.  */
CONS4_FN (4, int32_t);

/*
** cons4_8_int32_t:
**	...
**	stp	q[0-9]+, .*
**	ret
*/
CONS4_FN (8, int32_t);
