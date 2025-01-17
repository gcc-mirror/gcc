/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** uaddq:
** ...
**	ld1w\tz([0-9]+)\.s, .*
**	ld1w\tz([0-9]+)\.s, .*
**	uqadd\tz\2.s, z\1\.s, z\2\.s
** ...
**	ldr\tw([0-9]+), .*
**	ldr\tw([0-9]+), .*
**	adds\tw\3, w\3, w\4
**	csinv\tw\3, w\3, wzr, cc
** ...
*/
/*
** uaddq2:
** ...
**	ld1w\tz([0-9]+)\.s, .*
**	ld1w\tz([0-9]+)\.s, .*
**	uqadd\tz\2.s, z\1\.s, z\2\.s
** ...
**	ldr\tw([0-9]+), .*
**	ldr\tw([0-9]+), .*
**	adds\tw\3, w\3, w\4
**	csinv\tw\3, w\3, wzr, cc
** ...
*/
/*
** uaddq_imm:
** ...
**	ld1w\tz([0-9]+)\.s, .*
**	uqadd\tz\1.s, z\1\.s, #50
** ...
**	ldr\tw([0-9]+), .*
**	adds\tw\2, w\2, #50
**	csinv\tw\2, w\2, wzr, cc
** ...
*/
/*
** usubq: { xfail *-*-* }
** ...
**	ld1w\tz([0-9]+)\.s, .*
**	ld1w\tz([0-9]+)\.s, .*
**	uqsub\tz\2.s, z\1\.s, z\2\.s
** ...
**	ldr\tw([0-9]+), .*
**	ldr\tw([0-9]+), .*
**	subs\tw\3, w\3, w\4
**	csel\tw\3, w\3, wzr, cs
** ...
*/

#include <limits.h>

#define UT unsigned int
#define UMAX UINT_MAX
#define UMIN 0

#include "saturating_arithmetic.inc"