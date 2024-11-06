/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-options "-O2" } */

#include "arm_sve.h"

/*
** s64_x:
**	mov	z[0-9]+\.d, #20
**	ret
*/
svint64_t s64_x (svbool_t pg) {
    return svlsl_n_s64_x (pg, svdup_s64 (5), 2);  
}

/*
** s64_x_vect:
**	mov	z[0-9]+\.d, #20
**	ret
*/
svint64_t s64_x_vect (svbool_t pg) {
    return svlsl_s64_x (pg, svdup_s64 (5), svdup_u64 (2));  
}

/*
** s64_z:
**	mov	z[0-9]+\.d, p[0-7]/z, #20
**	ret
*/
svint64_t s64_z (svbool_t pg) {
    return svlsl_n_s64_z (pg, svdup_s64 (5), 2);  
}

/*
** s64_z_vect:
**	mov	z[0-9]+\.d, p[0-7]/z, #20
**	ret
*/
svint64_t s64_z_vect (svbool_t pg) {
    return svlsl_s64_z (pg, svdup_s64 (5), svdup_u64 (2));  
}

/*
** s64_m_ptrue:
**	mov	z[0-9]+\.d, #20
**	ret
*/
svint64_t s64_m_ptrue () {
    return svlsl_n_s64_m (svptrue_b64 (), svdup_s64 (5), 2);  
}

/*
** s64_m_ptrue_vect:
**	mov	z[0-9]+\.d, #20
**	ret
*/
svint64_t s64_m_ptrue_vect () {
    return svlsl_s64_m (svptrue_b64 (), svdup_s64 (5), svdup_u64 (2));  
}

/*
** s64_m_pg:
**	mov	z[0-9]+\.d, #5
**	lsl	z[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #2
**	ret
*/
svint64_t s64_m_pg (svbool_t pg) {
    return svlsl_n_s64_m (pg, svdup_s64 (5), 2);
} 

/*
** s64_m_pg_vect:
**	mov	z[0-9]+\.d, #5
**	lsl	z[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #2
**	ret
*/
svint64_t s64_m_pg_vect (svbool_t pg) {
    return svlsl_s64_m (pg, svdup_s64 (5), svdup_u64 (2));
}

/*
** s64_x_0:
**	mov	z[0-9]+\.d, #5
**	ret
*/
svint64_t s64_x_0 (svbool_t pg) {
    return svlsl_n_s64_x (pg, svdup_s64 (5), 0);  
}

/*
** s64_x_bit_width:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_bit_width (svbool_t pg) {
    return svlsl_n_s64_x (pg, svdup_s64 (5), 64); 
}

/*
** s64_x_out_of_range:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
svint64_t s64_x_out_of_range (svbool_t pg) {
    return svlsl_n_s64_x (pg, svdup_s64 (5), 68); 
}

/*
** u8_x_unsigned_overflow:
**	mov	z[0-9]+\.b, #-2
**	ret
*/
svuint8_t u8_x_unsigned_overflow (svbool_t pg) {
    return svlsl_n_u8_x (pg, svdup_u8 (255), 1);  
}

/*
** s8_x_signed_overflow:
**	mov	z[0-9]+\.b, #-2
**	ret
*/
svint8_t s8_x_signed_overflow (svbool_t pg) {
    return svlsl_n_s8_x (pg, svdup_s8 (255), 1);  
}

/*
** s8_x_neg_shift:
**	mov	z[0-9]+\.b, #-2
**	ret
*/
svint8_t s8_x_neg_shift (svbool_t pg) {
    return svlsl_n_s8_x (pg, svdup_s8 (-1), 1);  
}

/*
** s8_x_max_shift:
**	mov	z[0-9]+\.b, #-128
**	ret
*/
svint8_t s8_x_max_shift (svbool_t pg) {
    return svlsl_n_s8_x (pg, svdup_s8 (1), 7);  
}

