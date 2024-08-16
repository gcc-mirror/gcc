/* { dg-options "-O2" } */

#include <arm_neon.h>
#include <arm_sve.h>
#include <arm_neon_sve_bridge.h>

svuint16_t
convolve4_4_x (uint16x8x2_t permute_tbl)
{
    return svset_neonq_u16 (svundef_u16 (), permute_tbl.val[1]);
}

/* { dg-final { scan-assembler {\tmov\tz0\.d, z1\.d\n} } } */
