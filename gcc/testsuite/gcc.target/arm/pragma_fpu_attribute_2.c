/* Test for #pragma assembly extension generations.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-additional-options "-std=gnu99 -mfpu=vfpv3-d16" } */

#include <stdint.h>
#include <arm_neon.h>

extern uint32_t bar();

#pragma GCC push_options
#pragma GCC target("fpu=crypto-neon-fp-armv8")
poly64x1_t vsricw(poly64x1_t crc, uint32_t val)
{
    poly64x1_t res;
    asm("vsri %0, %1, %2" : "=r"(res) : "r"(crc), "r"(val));
    return res;
}
#pragma GCC pop_options

uint32_t restored ()
{
  return bar();
}

/* { dg-final { scan-assembler-times {\.fpu\s+crypto-neon-fp-armv8} 1 } } */
/* { dg-final { scan-assembler-times {\.fpu\s+vfpv3-d16} 1 } } */
