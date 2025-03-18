/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

/*
** uadd_lane:
**	fmov\tw([0-9]+), s0
**	adds\tw([0-9]+), (?:w\1, w0|w0, w\1)
**	csinv\tw\2, w\2, wzr, cc
**	ret
*/
/*
** uaddq:
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqadd\tv[0-9]+.4s, (?:v\1.4s, v\2.4s|v\2.4s, v\1.4s)
** ...
**	ldr\tw([0-9]+), .*
**	ldr\tw([0-9]+), .*
**	adds\tw([0-9]+), (?:w\3, w\4|w\4, w\3)
**	csinv\tw\5, w\5, wzr, cc
** ...
**	ldr\tw([0-9]+), .*
**	ldr\tw([0-9]+), .*
**	adds\tw([0-9]+), (?:w\6, w\7|w\7, w\6)
**	csinv\tw\8, w\8, wzr, cc
** ...
*/
/*
** uaddq2:
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqadd\tv[0-9]+.4s, (?:v\1.4s, v\2.4s|v\2.4s, v\1.4s)
** ...
**	ldr\tw([0-9]+), .*
**	ldr\tw([0-9]+), .*
**	adds\tw([0-9]+), (?:w\3, w\4|w\4, w\3)
**	csinv\tw\5, w\5, wzr, cc
** ...
**	ldr\tw([0-9]+), .*
**	ldr\tw([0-9]+), .*
**	adds\tw([0-9]+), (?:w\6, w\7|w\7, w\6)
**	csinv\tw\8, w\8, wzr, cc
** ...
*/
/*
** usubq: { xfail *-*-* }
** ...
**	ldr\tq([0-9]+), .*
**	ldr\tq([0-9]+), .*
**	uqsub\tv[0-9]+.4s, v\1.4s, v\2.4s
** ...
**	ldr\tw([0-9]+), .*
**	ldr\tw([0-9]+), .*
**	subs\tw([0-9]+), w\3, w\4
**	csel\tw\5, w\5, wzr, cs
** ...
**	ldr\tw([0-9]+), .*
**	ldr\tw([0-9]+), .*
**	subs\tw([0-9]+), w\6, w\7
**	csel\tw\8, w\8, wzr, cs
** ...
*/

#include <limits.h>
#include <arm_neon.h>

#define UT unsigned int
#define VT uint32x2_t
#define UMAX UINT_MAX
#define UMIN 0

#include "saturating_arithmetic_autovect.inc"