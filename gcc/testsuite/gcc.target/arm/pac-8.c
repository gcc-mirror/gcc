/* Testing return address signing.  */
/* { dg-do run } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_link } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-require-effective-target arm_pacbti_hw } */
/* { dg-options "-mbranch-protection=pac-ret+leaf -mfloat-abi=hard --save-temps -O0" } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */

#include <stdarg.h>
#include <stdlib.h>

int acc (int n, ...)
{
    int sum = 0;
    va_list ptr;

    va_start (ptr, n);

    for (int i = 0; i < n; i++)
        sum += va_arg (ptr, int);
    va_end (ptr);

    return sum;
}

int main()
{
  if (acc (3, 1, 2, 3) != 6)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "\tpac\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-times "\taut\tip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-not "\tbti" } } */
