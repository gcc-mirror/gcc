/* { dg-do link { target aarch64_asm_ls64_ok } } */
/* { dg-additional-options "-march=armv8.7-a -flto" } */
#include <arm_acle.h>
int main(void)
{
  data512_t d = __arm_ld64b ((const void *)0x1000);
  __arm_st64b ((void *)0x2000, d);
  uint64_t x = __arm_st64bv ((void *)0x3000, d);
  x += __arm_st64bv0 ((void *)0x4000, d);
}
