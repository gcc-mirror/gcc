/* { dg-do compile } */
/* { dg-additional-options "-O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon.h>
#include <arm_neon_sve_bridge.h>
#include <stdint.h>

/*
** sub_neon_i16_sve_bridged:
**	ssubl2	v0.8h, v0.16b, v1.16b
**	ret
*/
svint16_t sub_neon_i16_sve_bridged(svint8_t a, svint8_t b) {
    return svset_neonq_s16(svundef_s16(),
            vsubq_s16(vmovl_high_s8(svget_neonq(a)),
                      vmovl_high_s8(svget_neonq(b))));
}

