/* { dg-do compile } */
/* { dg-additional-options "-O -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** cntd_pow2:
**	mov	x0, #?4
**	ret
*/
uint64_t cntd_pow2 () { return svcntd_pat (SV_POW2); }

/*
** cntd_vl1:
**	mov	x0, #?1
**	ret
*/
uint64_t cntd_vl1 () { return svcntd_pat (SV_VL1); }

/*
** cntd_vl2:
**	mov	x0, #?2
**	ret
*/
uint64_t cntd_vl2 () { return svcntd_pat (SV_VL2); }

/*
** cntd_vl3:
**	mov	x0, #?3
**	ret
*/
uint64_t cntd_vl3 () { return svcntd_pat (SV_VL3); }

/*
** cntd_vl4:
**	mov	x0, #?4
**	ret
*/
uint64_t cntd_vl4 () { return svcntd_pat (SV_VL4); }

/*
** cntd_vl5:
**	mov	x0, #?0
**	ret
*/
uint64_t cntd_vl5 () { return svcntd_pat (SV_VL5); }

/*
** cntd_vl6:
**	mov	x0, #?0
**	ret
*/
uint64_t cntd_vl6 () { return svcntd_pat (SV_VL6); }

/*
** cntd_vl7:
**	mov	x0, #?0
**	ret
*/
uint64_t cntd_vl7 () { return svcntd_pat (SV_VL7); }

/*
** cntd_vl8:
**	mov	x0, #?0
**	ret
*/
uint64_t cntd_vl8 () { return svcntd_pat (SV_VL8); }

/*
** cntd_vl16:
**	mov	x0, #?0
**	ret
*/
uint64_t cntd_vl16 () { return svcntd_pat (SV_VL16); }

/*
** cntd_vl32:
**	mov	x0, #?0
**	ret
*/
uint64_t cntd_vl32 () { return svcntd_pat (SV_VL32); }

/*
** cntd_vl64:
**	mov	x0, #?0
**	ret
*/
uint64_t cntd_vl64 () { return svcntd_pat (SV_VL64); }

/*
** cntd_vl128:
**	mov	x0, #?0
**	ret
*/
uint64_t cntd_vl128 () { return svcntd_pat (SV_VL128); }

/*
** cntd_vl256:
**	mov	x0, #?0
**	ret
*/
uint64_t cntd_vl256 () { return svcntd_pat (SV_VL256); }

/*
** cntd_mul3:
**	mov	x0, #?3
**	ret
*/
uint64_t cntd_mul3 () { return svcntd_pat (SV_MUL3); }

/*
** cntd_mul4:
**	mov	x0, #?4
**	ret
*/
uint64_t cntd_mul4 () { return svcntd_pat (SV_MUL4); }

/*
** cntd_all:
**	mov	x0, #?4
**	ret
*/
uint64_t cntd_all () { return svcntd_pat (SV_ALL); }

#ifdef __cplusplus
}
#endif
