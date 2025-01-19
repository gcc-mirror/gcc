/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** uaddq:
** ...
**	ld1d\tz([0-9]+)\.d, .*
**	ld1d\tz([0-9]+)\.d, .*
**	uqadd\tz\2.d, z\1\.d, z\2\.d
** ...
**	ldr\tx([0-9]+), .*
**	ldr\tx([0-9]+), .*
**	adds\tx\3, x\3, x\4
**	csinv\tx\3, x\3, xzr, cc
** ...
*/
/*
** uaddq2:
** ...
**	ld1d\tz([0-9]+)\.d, .*
**	ld1d\tz([0-9]+)\.d, .*
**	uqadd\tz\2.d, z\1\.d, z\2\.d
** ...
**	ldr\tx([0-9]+), .*
**	ldr\tx([0-9]+), .*
**	adds\tx\3, x\3, x\4
**	csinv\tx\3, x\3, xzr, cc
** ...
*/
/*
** uaddq_imm:
** ...
**	ld1d\tz([0-9]+)\.d, .*
**	uqadd\tz\1.d, z\1\.d, #50
** ...
**	ldr\tx([0-9]+), .*
**	adds\tx\2, x\2, #50
**	csinv\tx\2, x\2, xzr, cc
** ...
*/
/*
** usubq: { xfail *-*-* }
** ...
**	ld1d\tz([0-9]+)\.d, .*
**	ld1d\tz([0-9]+)\.d, .*
**	uqsub\tz\2.d, z\1\.d, z\2\.d
** ...
**	ldr\tx([0-9]+), .*
**	ldr\tx([0-9]+), .*
**	subs\tx\3, x\3, x\4
**	csel\tx\3, x\3, xzr, cs
** ...
*/

#include <limits.h>

#define UT unsigned long
#define UMAX ULONG_MAX
#define UMIN 0

#include "saturating_arithmetic.inc"