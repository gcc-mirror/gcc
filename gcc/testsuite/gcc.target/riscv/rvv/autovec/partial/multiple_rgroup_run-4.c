/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=zvl" } */

#include "multiple_rgroup-4.c"

int __attribute__ ((optimize (0))) main (void)
{
  uint64_t f_x[3108], f_x2[3108];
  uint16_t f_y[6216], f_y2[6216];
  f_init (f_x, f_x2, f_y, f_y2, 3108);
  f (f_x, f_y, 3108);
  f_golden (f_x2, f_y2, 3108);
  f_check (f_x, f_x2, f_y, f_y2, 3108);
  return 0;
}
