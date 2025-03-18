/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

/*
** uadd_lane:
** ...
**	(?:fmov|ldr)\tx([0-9]+), .*
** ...
**	adds\tx([0-9]+), (?:x\1, x0|x0, x\1)
**	csinv\tx\2, x\2, xzr, cc
**	ret
*/
/*
** uaddq:
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqadd\tv[0-9]+.2d, (?:v\1.2d, v\2.2d|v\2.2d, v\1.2d)
** ...
**	ldr\tx([0-9]+), .*
**	ldr\tx([0-9]+), .*
**	adds\tx([0-9]+), (?:x\3, x\4|x\4, x\3)
**	csinv\tx\5, x\5, xzr, cc
** ...
**	ldr\tx([0-9]+), .*
**	ldr\tx([0-9]+), .*
**	adds\tx([0-9]+), (?:x\6, x\7|x\7, x\6)
**	csinv\tx\8, x\8, xzr, cc
** ...
*/
/*
** uaddq2:
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqadd\tv[0-9]+.2d, (?:v\1.2d, v\2.2d|v\2.2d, v\1.2d)
** ...
**	ldr\tx([0-9]+), .*
**	ldr\tx([0-9]+), .*
**	adds\tx([0-9]+), (?:x\3, x\4|x\4, x\3)
**	csinv\tx\5, x\5, xzr, cc
** ...
**	ldr\tx([0-9]+), .*
**	ldr\tx([0-9]+), .*
**	adds\tx([0-9]+), (?:x\6, x\7|x\7, x\6)
**	csinv\tx\8, x\8, xzr, cc
** ...
*/
/*
** usubq: { xfail *-*-* }
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqsub\tv[0-9]+.2d, v\1.2d, v\2.2d
** ...
**	ldr\tx([0-9]+), .*
**	ldr\tx([0-9]+), .*
**	subs\tx([0-9]+), x\3, x\4
**	csel\tx\5, x\5, xzr, cs
** ...
**	ldr\tx([0-9]+), .*
**	ldr\tx([0-9]+), .*
**	subs\tx([0-9]+), x\6, x\7
**	csel\tx\8, x\8, xzr, cs
** ...
*/

#include <limits.h>
#include <arm_neon.h>

#define UT unsigned long
#define VT uint64x2_t
#define UMAX ULONG_MAX
#define UMIN 0

#include "saturating_arithmetic_autovect.inc"