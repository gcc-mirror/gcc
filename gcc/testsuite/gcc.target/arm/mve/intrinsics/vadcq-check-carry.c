/* { dg-do run } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

#include <inttypes.h>
#include <stdio.h>

__attribute((noinline)) void print_uint32x4_t(const char *name, uint32x4_t val)
{
  printf("%s: %u, %u, %u, %u\n",
	 name,
	 vgetq_lane_u32(val, 0),
         vgetq_lane_u32(val, 1),
	 vgetq_lane_u32(val, 2),
         vgetq_lane_u32(val, 3));
}

void __attribute__ ((noinline))  test_2(void)
{
  uint32x4_t v12, v18, v108;
  unsigned v17 = 0;
  v12 = vdupq_n_u32(1);
  v18 = vadcq_u32(v12, v12, &v17);
  v17 = 1;
  v108 = vadcq_u32(v12, v12, &v17);
  print_uint32x4_t("v108", v108);
}

int main()
{
  test_2();
  return 0;
}
  
#ifdef __cplusplus
}
#endif

/* { dg-output "v108: 3, 2, 2, 2" } */
/* { dg-final { scan-assembler-times {\tvmrs\t(?:ip|fp|r[0-9]+), FPSCR_nzcvqc} 3 } } */
