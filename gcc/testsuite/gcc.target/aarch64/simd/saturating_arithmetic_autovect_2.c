/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

/*
** uadd_lane: { xfail *-*-* }
**	dup\tv([0-9]+).4h, w0
**	uqadd\th([0-9]+), (?:h\1, h0|h0, h\1)
**	umov\tw0, v\2.h\[0\]
**	ret
*/
/*
** uaddq:
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqadd\tv[0-9]+.8h, (?:v\1.8h, v\2.8h|v\2.8h, v\1.8h)
** ...
**	ldr\td([0-9]+), .*
**	ldr\td([0-9]+), .*
**	uqadd\tv[0-9]+.4h, (?:v\3.4h, v\4.4h|v\4.4h, v\3.4h)
** ...
**	ldr\th([0-9]+), .*
**	ldr\th([0-9]+), .*
**	uqadd\th[0-9]+, (?:h\5, h\6|h\6, h\5)
** ...
**	ldr\th([0-9]+), .*
**	ldr\th([0-9]+), .*
**	uqadd\th[0-9]+, (?:h\7, h\8|h\8, h\7)
** ...
*/
/*
** uaddq2:
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqadd\tv[0-9]+.8h, (?:v\1.8h, v\2.8h|v\2.8h, v\1.8h)
** ...
**	ldr\td([0-9]+), .*
**	ldr\td([0-9]+), .*
**	uqadd\tv[0-9]+.4h, (?:v\3.4h, v\4.4h|v\4.4h, v\3.4h)
** ...
**	ldr\th([0-9]+), .*
**	ldr\th([0-9]+), .*
**	uqadd\th[0-9]+, (?:h\5, h\6|h\6, h\5)
** ...
**	uqadd\th([0-9]+), (?:h[0-9]+, h\7|h\7, h[0-9]+)
** ...
*/
/*
** usubq: { xfail *-*-* }
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqsub\tv[0-9]+.8h, v\1.8h, v\2.8h
** ...
**	ldr\td([0-9]+), .*
**	ldr\td([0-9]+), .*
**	uqsub\tv[0-9]+.4h, v\3.4h, v\4.4h
** ...
**	ldr\th([0-9]+), .*
**	ldr\th([0-9]+), .*
**	uqsub\th[0-9]+, h\5, h\6
** ...
**	ldr\th([0-9]+), .*
**	ldr\th([0-9]+), .*
**	uqsub\th[0-9]+, h\7, h\8
** ...
*/

#include <limits.h>
#include <arm_neon.h>

#define UT unsigned short
#define VT uint16x4_t
#define UMAX USHRT_MAX
#define UMIN 0

#include "saturating_arithmetic_autovect.inc"