/* { dg-do run } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve } */

#include "arm_mve.h"
#include "stdio.h"
#include <stdlib.h>

#define IMM 3

void
foo (int64_t acc, uint64_t acc1)
{
  acc = sqshll (acc, IMM);
  if (acc != 128)
    abort();
  acc = srshrl (acc, IMM);
  if (acc != 16)
    abort();
  acc1 = uqshll (acc1, IMM);
  if (acc1 != 128)
    abort();
  acc1 = urshrl (acc1, IMM);
  if (acc1 != 16)
    abort();
}

int main()
{
  int64_t acc = 16;
  uint64_t acc1 = 16;
  foo (acc, acc1);
  return 0;
}
