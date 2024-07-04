/* { dg-do run } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#include <arm_mve.h>

__attribute__((noipa))
uint32x4_t foo (void) {
  uint32x4_t V0 = vld1q_u32(((const uint32_t[4]){1, 2, 3, 4}));
  return V0;
}

int main(void)
{
  uint32_t buf[4];
 vst1q_u32 (buf, foo());

  for (int i = 0; i < 4; i++)
    if (buf[i] != i+1)
      __builtin_abort ();
}
