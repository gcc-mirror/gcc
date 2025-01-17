/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

/*
** uadd_lane: { xfail *-*-* }
**	dup\tv([0-9]+).8b, w0
**	uqadd\tb([0-9]+), (?:b\1, b0|b0, b\1)
**	umov\tw0, v\2.b\[0\]
**	ret
*/
/*
** uaddq:
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqadd\tv[0-9]+.16b, (?:v\1.16b, v\2.16b|v\2.16b, v\1.16b)
** ...
**	ldr\td([0-9]+), .*
**	ldr\td([0-9]+), .*
**	uqadd\tv[0-9]+.8b, (?:v\3.8b, v\4.8b|v\4.8b, v\3.8b)
** ...
**	ldr\tb([0-9]+), .*
**	ldr\tb([0-9]+), .*
**	uqadd\tb[0-9]+, (?:b\5, b\6|b\6, b\5)
** ...
**	ldr\tb([0-9]+), .*
**	ldr\tb([0-9]+), .*
**	uqadd\tb[0-9]+, (?:b\7, b\8|b\8, b\7)
** ...
*/
/*
** uaddq2:
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqadd\tv[0-9]+.16b, (?:v\1.16b, v\2.16b|v\2.16b, v\1.16b)
** ...
**	ldr\td([0-9]+), .*
**	ldr\td([0-9]+), .*
**	uqadd\tv[0-9]+.8b, (?:v\3.8b, v\4.8b|v\4.8b, v\3.8b)
** ...
**	ldr\tb([0-9]+), .*
**	ldr\tb([0-9]+), .*
**	uqadd\tb[0-9]+, (?:b\5, b\6|b\6, b\5)
** ...
**	uqadd\tb([0-9]+), (?:b[0-9]+, b\7|b\7, b[0-9]+)
** ...
*/
/*
** usubq: { xfail *-*-* }
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqsub\tv[0-9]+.16b, v\1.16b, v\2.16b
** ...
**	ldr\td([0-9]+), .*
**	ldr\td([0-9]+), .*
**	uqsub\tv[0-9]+.8b, v\3.8b, v\4.8b
** ...
**	ldr\tb([0-9]+), .*
**	ldr\tb([0-9]+), .*
**	uqsub\tb[0-9]+, b\5, b\6
** ...
**	ldr\tb([0-9]+), .*
**	ldr\tb([0-9]+), .*
**	uqsub\tb[0-9]+, b\7, b\8
** ...
*/

#include <limits.h>
#include <arm_neon.h>

#define UT unsigned char
#define VT uint8x8_t
#define UMAX UCHAR_MAX
#define UMIN 0

#include "saturating_arithmetic_autovect.inc"