/* Check if the optimizers are not removing the umulsihi3_imm
   instruction.  */
/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */

#include <stdint.h>

static int32_t test (int16_t reg_val)
{
  int32_t x = (reg_val & 0xf) * 62500;
  return x;
}

int main (void)
{
  volatile int32_t x = 0xc172;
  x = test (x);

  if (x != 0x0001e848)
    __builtin_abort ();
  return 0;
}

