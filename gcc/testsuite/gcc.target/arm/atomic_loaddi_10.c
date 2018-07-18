/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7ve_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v7ve } */

#include <stdatomic.h>

atomic_llong x = 0;

atomic_llong get_x()
{
  return atomic_load(&x);
}

/* { dg-final { scan-assembler "ldrd" } } */
