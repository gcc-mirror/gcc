/* testsuite/gcc.target/arm/pr48183.c */

/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O -g" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

void move_16bit_to_32bit (int32_t *dst, const short *src, unsigned n)
{
    unsigned i;
    int16x4x2_t input;
    int32x4x2_t mid;
    int32x4x2_t output;

    for (i = 0; i < n/2; i += 8) {
        input = vld2_s16(src + i);
        mid.val[0] = vmovl_s16(input.val[0]);
        mid.val[1] = vmovl_s16(input.val[1]);
        output.val[0] = vshlq_n_s32(mid.val[0], 8);
        output.val[1] = vshlq_n_s32(mid.val[1], 8);
        vst2q_s32((int32_t *)dst + i, output);
    }
}
