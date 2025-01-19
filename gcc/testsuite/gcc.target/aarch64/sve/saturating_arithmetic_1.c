/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** uaddq:
** ...
**	ld1b\tz([0-9]+)\.b, .*
**	ld1b\tz([0-9]+)\.b, .*
**	uqadd\tz\2.b, z\1\.b, z\2\.b
** ...
**	ldr\tb([0-9]+), .*
**	ldr\tb([0-9]+), .*
**	uqadd\tb\4, b\3, b\4
** ...
*/
/*
** uaddq2:
** ...
**	ld1b\tz([0-9]+)\.b, .*
**	ld1b\tz([0-9]+)\.b, .*
**	uqadd\tz\2.b, z\1\.b, z\2\.b
** ...
**	ldr\tb([0-9]+), .*
**	ldr\tb([0-9]+), .*
**	uqadd\tb\4, b\3, b\4
** ...
*/
/*
** uaddq_imm:
** ...
**	ld1b\tz([0-9]+)\.b, .*
**	uqadd\tz\1.b, z\1\.b, #50
** ...
**	movi\tv([0-9]+)\.8b, 0x32
** ...
**	ldr\tb([0-9]+), .*
**	uqadd\tb\3, b\3, b\2
** ...
*/
/*
** usubq: { xfail *-*-* }
** ...
**	ld1b\tz([0-9]+)\.b, .*
**	ld1b\tz([0-9]+)\.b, .*
**	uqsub\tz\2.b, z\1\.b, z\2\.b
** ...
**	ldr\tb([0-9]+), .*
**	ldr\tb([0-9]+), .*
**	uqsub\tb\4, b\3, b\4
** ...
*/

#include <limits.h>

#define UT unsigned char
#define UMAX UCHAR_MAX
#define UMIN 0

#include "saturating_arithmetic.inc"