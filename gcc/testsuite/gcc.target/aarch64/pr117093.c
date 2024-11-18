/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-options "-O1" } */

#include <arm_neon.h>

/*
** foo:
**	rev64	v0\.4s, v0\.4s
**	ret
*/
uint64x2_t foo (uint64x2_t r) {
    uint32x4_t a = vreinterpretq_u32_u64 (r);
    uint32_t t;
    t = a[0]; a[0] = a[1]; a[1] = t;
    t = a[2]; a[2] = a[3]; a[3] = t;
    return vreinterpretq_u64_u32 (a);
}
