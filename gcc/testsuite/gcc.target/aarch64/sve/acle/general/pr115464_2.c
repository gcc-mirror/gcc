/* { dg-options "-O2" } */

#include <arm_neon.h>
#include <arm_sve.h>
#include <arm_neon_sve_bridge.h>

svuint16_t
convolve4_4_x (uint16x8x2_t permute_tbl, svuint16_t a)
{
    return svset_neonq_u16 (a, permute_tbl.val[1]);
}
