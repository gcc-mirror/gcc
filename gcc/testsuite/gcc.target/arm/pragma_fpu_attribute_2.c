/* Test for #pragma assembly extension generations.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-add-options arm_fp } */

#include <stdint.h>

#pragma GCC target("fpu=vfpv3-d16")

extern uint32_t bar();

#pragma GCC push_options
#pragma GCC target("fpu=vfpv4")
extern float fmaf (float, float, float);

float
vfma32 (float x, float y, float z)
{
  return fmaf (x, y, z);
}
#pragma GCC pop_options

uint32_t restored ()
{
  return bar();
}

/* { dg-final { scan-assembler-times {\.fpu\s+vfpv4} 1 } } */
/* { dg-final { scan-assembler-times {\.fpu\s+vfpv3-d16} 1 } } */
