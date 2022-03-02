/* { dg-do run } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O3" } */
/* { dg-add-options arm_v8_1m_mve } */

#include <arm_mve.h>


__attribute((noipa))
unsigned foo(int8x16_t v, int8x16_t w)
{
  return vcmpeqq (v, w);
}

int main(void)
{
  if (foo (vdupq_n_s8(0), vdupq_n_s8(0)) != 0xffffU)
    __builtin_abort ();
}
