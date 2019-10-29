/* { dg-do compile } */
/* { dg-additional-options "-O -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** cnth_pow2:
**	mov	x0, #?16
**	ret
*/
uint64_t cnth_pow2 () { return svcnth_pat (SV_POW2); }

/*
** cnth_vl1:
**	mov	x0, #?1
**	ret
*/
uint64_t cnth_vl1 () { return svcnth_pat (SV_VL1); }

/*
** cnth_vl2:
**	mov	x0, #?2
**	ret
*/
uint64_t cnth_vl2 () { return svcnth_pat (SV_VL2); }

/*
** cnth_vl3:
**	mov	x0, #?3
**	ret
*/
uint64_t cnth_vl3 () { return svcnth_pat (SV_VL3); }

/*
** cnth_vl4:
**	mov	x0, #?4
**	ret
*/
uint64_t cnth_vl4 () { return svcnth_pat (SV_VL4); }

/*
** cnth_vl5:
**	mov	x0, #?5
**	ret
*/
uint64_t cnth_vl5 () { return svcnth_pat (SV_VL5); }

/*
** cnth_vl6:
**	mov	x0, #?6
**	ret
*/
uint64_t cnth_vl6 () { return svcnth_pat (SV_VL6); }

/*
** cnth_vl7:
**	mov	x0, #?7
**	ret
*/
uint64_t cnth_vl7 () { return svcnth_pat (SV_VL7); }

/*
** cnth_vl8:
**	mov	x0, #?8
**	ret
*/
uint64_t cnth_vl8 () { return svcnth_pat (SV_VL8); }

/*
** cnth_vl16:
**	mov	x0, #?16
**	ret
*/
uint64_t cnth_vl16 () { return svcnth_pat (SV_VL16); }

/*
** cnth_vl32:
**	mov	x0, #?0
**	ret
*/
uint64_t cnth_vl32 () { return svcnth_pat (SV_VL32); }

/*
** cnth_vl64:
**	mov	x0, #?0
**	ret
*/
uint64_t cnth_vl64 () { return svcnth_pat (SV_VL64); }

/*
** cnth_vl128:
**	mov	x0, #?0
**	ret
*/
uint64_t cnth_vl128 () { return svcnth_pat (SV_VL128); }

/*
** cnth_vl256:
**	mov	x0, #?0
**	ret
*/
uint64_t cnth_vl256 () { return svcnth_pat (SV_VL256); }

/*
** cnth_mul3:
**	mov	x0, #?15
**	ret
*/
uint64_t cnth_mul3 () { return svcnth_pat (SV_MUL3); }

/*
** cnth_mul4:
**	mov	x0, #?16
**	ret
*/
uint64_t cnth_mul4 () { return svcnth_pat (SV_MUL4); }

/*
** cnth_all:
**	mov	x0, #?16
**	ret
*/
uint64_t cnth_all () { return svcnth_pat (SV_ALL); }

#ifdef __cplusplus
}
#endif
