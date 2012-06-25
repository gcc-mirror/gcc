/* { dg-do run } */
/* { dg-require-effective-target arm_eabi } */
/* { dg-require-effective-target arm_arch_v5t_multilib } */
/* { dg-options "-mthumb" } */
/* { dg-add-options arm_arch_v5t } */

#include "ftest-support-thumb.h"

int
main (void)
{
  return ftest (ARCH_V5T);
}

