/* { dg-do run } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve } */

#include "arm_mve.h"
#include "stdio.h"
#include <stdlib.h>

void
foo (int32_t acc, uint32_t acc1, int shift)
{
  acc = sqrshr (acc, shift);
  if (acc != 16)
    abort();
  acc1 = uqrshl (acc1, shift);
  if (acc1 != 128)
    abort();
}

int main()
{
  int32_t acc = 128;
  uint32_t acc1 = 16;
  int shift = 3;
  foo (acc, acc1, shift);
  return 0;
}
