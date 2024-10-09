/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

#pragma GCC target "+sha3"

static inline uint64x2_t
rotr64_vec(uint64x2_t x, const int b)
{
    int64x2_t neg_b = vdupq_n_s64(-b);
    int64x2_t left_shift = vsubq_s64(vdupq_n_s64(64), vdupq_n_s64(b));
    
    uint64x2_t right_shifted = vshlq_u64(x, neg_b);
    uint64x2_t left_shifted = vshlq_u64(x, left_shift);
    
    return vorrq_u64(right_shifted, left_shifted);
}

void G(
    int64_t* v,
    int64x2_t& m1_01, 
    int64x2_t& m1_23, 
    int64x2_t& m2_01, 
    int64x2_t& m2_23   
) {
    int64x2_t vd01 = {v[12],v[13]};
    vd01 = veorq_s64(vd01, m1_01);
    vd01 = vreinterpretq_s64_u64(rotr64_vec( vreinterpretq_u64_s64 (vd01), 32));
    v[12] = vgetq_lane_s64(vd01, 0);
}

/* { dg-final { scan-assembler {\txar\tv[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d, 32\n} } } */

