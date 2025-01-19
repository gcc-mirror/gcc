/* Testing return address signing.  */
/* { dg-do run } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_link } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-require-effective-target arm_pacbti_hw } */
/* { dg-options "-mbranch-protection=pac-ret+leaf -mfloat-abi=hard --save-temps -O0" } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */

#include <stdlib.h>

int
__attribute__((noinline))
foo1 (int a, int b)
{
  int x = 4;
  int foo2 (int a, int b)
  {
    return a + b + x;
  }
  return foo2 (a, b);
}

int
main (void)
{
  if (foo1 (1, 2) != 7)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "\tpac\tip, lr, sp" 3 } } */
/* { dg-final { scan-assembler-times "\taut\tip, lr, sp" 3 } } */
/* { dg-final { scan-assembler-not "\tbti" } } */
