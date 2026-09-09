/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

#include <arm_sve.h>
#pragma GCC target "+sve"

float tt[4];
float t2[4];
void ComputeCoefficients() {
    float block[4];
    {
        svbool_t t = svwhilelt_b32_u32({}, 4);
        svfloat32_t v = svld1_f32(t, tt);
        svst1_f32(t, block, v);
    }
    __builtin_memcpy(tt, block, sizeof(block));
    {
        svbool_t t = svwhilelt_b32_u32({}, 1);
        svfloat32_t v = svld1_f32(t, tt);
        svst1_f32(t, t2, v);
    }
}
