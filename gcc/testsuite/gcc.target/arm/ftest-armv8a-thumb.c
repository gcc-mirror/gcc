/* { dg-do run } */
/* { dg-require-effective-target arm_eabi } */
/* { dg-require-effective-target arm_arch_v8a_multilib } */
/* { dg-options "-mthumb" } */
/* { dg-add-options arm_arch_v8a } */

#include "ftest-support-thumb.h"

int
main (void)
{
  return ftest (ARCH_V8A);
}

