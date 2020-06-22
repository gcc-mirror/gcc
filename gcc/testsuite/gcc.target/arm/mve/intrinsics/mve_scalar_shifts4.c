/* { dg-do run } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve } */

#include "arm_mve.h"
#include <stdlib.h>

#define IMM 3

void
foo (int32_t acc,  uint32_t acc1)
{
  acc = sqshl (acc, IMM);
  if (acc != 128)
    abort();
  acc = srshr (acc, IMM);
  if (acc != 16)
    abort();
  acc1 = uqshl (acc1, IMM);
  if (acc1 != 128)
    abort();
  acc1 = urshr (acc1, IMM);
  if (acc1 != 16)
    abort();
}

int main()
{
  int32_t acc = 16;
  uint32_t acc1 = 16;
  foo (acc, acc1);
  return 0;
}
