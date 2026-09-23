// { dg-options "-O3 -fomit-frame-pointer -fno-optimize-sibling-calls" }

#include <arm_sme.h>

#pragma GCC target "+sme2+sme-lutv2"

__arm_streaming __arm_inout("zt0")
void foo(svfloat32_t z)
{
  svzero_zt (0);
  svwrite_lane_zt_f32 (0, z, 1);
}

// { dg-final { scan-assembler "zero" } }
