/* { dg-do run } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve } */

#include "arm_mve.h"
#include "stdio.h"
#include <stdlib.h>

void
foo (int64_t acc, int shift)
{
  acc = sqrshrl_sat48 (acc, shift);
  if (acc != 16)
    abort();
  acc = sqrshrl (acc, shift);
  if (acc != 2)
    abort();
}

void
foo1 (uint64_t acc, int shift)
{
  acc = uqrshll_sat48 (acc, shift);
  if (acc != 16)
    abort();
  acc = uqrshll (acc, shift);
  if (acc != 128)
    abort();
}

int main()
{
  int64_t acc = 128;
  uint64_t acc1 = 2;
  int shift = 3;
  foo (acc, shift);
  foo1 (acc1, shift);
  return 0;
}
