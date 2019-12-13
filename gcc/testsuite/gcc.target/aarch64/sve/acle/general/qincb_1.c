/* { dg-do compile } */
/* { dg-additional-options "-O" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** qincb_s32_s:
**	sqincb	x0, w0, all, mul #15
**	ret
*/
uint64_t qincb_s32_s (int32_t x) { return svqincb (x, 15); }

/*
** qincb_s32_z:
**	sqincb	x([0-9]+), w0, all, mul #15
**	uxtw	x0, w\1
**	ret
*/
uint64_t qincb_s32_z (int32_t x) { return (uint32_t) svqincb (x, 15); }

/*
** qincb_u32_s:
**	uqincb	(w[0-9]+), all, mul #15
**	sxtw	x0, \1
**	ret
*/
uint64_t qincb_u32_s (uint32_t x) { return (int32_t) svqincb (x, 15); }

/*
** qincb_u32_z:
**	uqincb	w0, all, mul #15
**	ret
*/
uint64_t qincb_u32_z (uint32_t x) { return svqincb (x, 15); }

#ifdef __cplusplus
}
#endif
