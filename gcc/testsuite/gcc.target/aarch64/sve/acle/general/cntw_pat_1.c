/* { dg-do compile } */
/* { dg-additional-options "-O -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** cntw_pow2:
**	mov	x0, #?8
**	ret
*/
uint64_t cntw_pow2 () { return svcntw_pat (SV_POW2); }

/*
** cntw_vl1:
**	mov	x0, #?1
**	ret
*/
uint64_t cntw_vl1 () { return svcntw_pat (SV_VL1); }

/*
** cntw_vl2:
**	mov	x0, #?2
**	ret
*/
uint64_t cntw_vl2 () { return svcntw_pat (SV_VL2); }

/*
** cntw_vl3:
**	mov	x0, #?3
**	ret
*/
uint64_t cntw_vl3 () { return svcntw_pat (SV_VL3); }

/*
** cntw_vl4:
**	mov	x0, #?4
**	ret
*/
uint64_t cntw_vl4 () { return svcntw_pat (SV_VL4); }

/*
** cntw_vl5:
**	mov	x0, #?5
**	ret
*/
uint64_t cntw_vl5 () { return svcntw_pat (SV_VL5); }

/*
** cntw_vl6:
**	mov	x0, #?6
**	ret
*/
uint64_t cntw_vl6 () { return svcntw_pat (SV_VL6); }

/*
** cntw_vl7:
**	mov	x0, #?7
**	ret
*/
uint64_t cntw_vl7 () { return svcntw_pat (SV_VL7); }

/*
** cntw_vl8:
**	mov	x0, #?8
**	ret
*/
uint64_t cntw_vl8 () { return svcntw_pat (SV_VL8); }

/*
** cntw_vl16:
**	mov	x0, #?0
**	ret
*/
uint64_t cntw_vl16 () { return svcntw_pat (SV_VL16); }

/*
** cntw_vl32:
**	mov	x0, #?0
**	ret
*/
uint64_t cntw_vl32 () { return svcntw_pat (SV_VL32); }

/*
** cntw_vl64:
**	mov	x0, #?0
**	ret
*/
uint64_t cntw_vl64 () { return svcntw_pat (SV_VL64); }

/*
** cntw_vl128:
**	mov	x0, #?0
**	ret
*/
uint64_t cntw_vl128 () { return svcntw_pat (SV_VL128); }

/*
** cntw_vl256:
**	mov	x0, #?0
**	ret
*/
uint64_t cntw_vl256 () { return svcntw_pat (SV_VL256); }

/*
** cntw_mul3:
**	mov	x0, #?6
**	ret
*/
uint64_t cntw_mul3 () { return svcntw_pat (SV_MUL3); }

/*
** cntw_mul4:
**	mov	x0, #?8
**	ret
*/
uint64_t cntw_mul4 () { return svcntw_pat (SV_MUL4); }

/*
** cntw_all:
**	mov	x0, #?8
**	ret
*/
uint64_t cntw_all () { return svcntw_pat (SV_ALL); }

#ifdef __cplusplus
}
#endif
