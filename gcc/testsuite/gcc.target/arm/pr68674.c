/* PR target/68674 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-require-effective-target arm_libc_fp_abi_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v7a } */
/* { dg-add-options arm_libc_fp_abi } */

#pragma GCC target ("fpu=vfp")

#include <arm_neon.h>

int8x8_t a;
extern int8x8_t b;
int16x8_t e;

void __attribute__((target("fpu=neon")))
foo1(void)
{
  e = (int16x8_t) vaddl_s8(a, b);
}

int8x8_t __attribute__((target("fpu=neon")))
foo2(void)
{
  return b;
}


