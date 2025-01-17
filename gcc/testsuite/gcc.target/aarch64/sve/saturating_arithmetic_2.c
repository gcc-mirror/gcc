/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** uaddq:
** ...
**	ld1h\tz([0-9]+)\.h, .*
**	ld1h\tz([0-9]+)\.h, .*
**	uqadd\tz\2.h, z\1\.h, z\2\.h
** ...
**	ldr\th([0-9]+), .*
**	ldr\th([0-9]+), .*
**	uqadd\th\4, h\3, h\4
** ...
*/
/*
** uaddq2:
** ...
**	ld1h\tz([0-9]+)\.h, .*
**	ld1h\tz([0-9]+)\.h, .*
**	uqadd\tz\2.h, z\1\.h, z\2\.h
** ...
**	ldr\th([0-9]+), .*
**	ldr\th([0-9]+), .*
**	uqadd\th\4, h\3, h\4
** ...
*/
/*
** uaddq_imm:
** ...
**	ld1h\tz([0-9]+)\.h, .*
**	uqadd\tz\1.h, z\1\.h, #50
** ...
**	movi\tv([0-9]+)\.4h, 0x32
** ...
**	ldr\th([0-9]+), .*
**	uqadd\th\3, h\3, h\2
** ...
*/
/*
** usubq: { xfail *-*-* }
** ...
**	ld1h\tz([0-9]+)\.h, .*
**	ld1h\tz([0-9]+)\.h, .*
**	usubq\tz\2.h, z\1\.h, z\2\.h
** ...
**	ldr\th([0-9]+), .*
**	ldr\th([0-9]+), .*
**	usubq\th\4, h\3, h\4
** ...
*/

#include <limits.h>

#define UT unsigned short
#define UMAX USHRT_MAX
#define UMIN 0

#include "saturating_arithmetic.inc"