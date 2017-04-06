/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7r_ok } */
/* { dg-skip-if "do not override -mcpu" { *-*-* } { "-mcpu=*" "-march=*" } { "-mcpu=cortex-r5" } } */
/* { dg-options "-O2 -mcpu=cortex-r5" } */

#include <stdatomic.h>

atomic_llong x = 0;

atomic_llong get_x()
{
  return atomic_load(&x);
}

/* { dg-final { scan-assembler-not "ldrd" } } */
