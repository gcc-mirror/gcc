/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=zvl" } */

#include "multiple_rgroup-3.c"

int __attribute__ ((optimize (0))) main (void)
{
  int8_t f0_x[3108], f0_x2[3108];
  int16_t f0_y[6216], f0_y2[6216];
  f0_init (f0_x, f0_x2, f0_y, f0_y2, 3108);
  f0 (f0_x, f0_y, 3108);
  f0_golden (f0_x2, f0_y2, 3108);
  f0_check (f0_x, f0_x2, f0_y, f0_y2, 3108);

  int16_t f1_x[1998], f1_x2[1998];
  int32_t f1_y[3996], f1_y2[3996];
  f1_init (f1_x, f1_x2, f1_y, f1_y2, 1998);
  f1 (f1_x, f1_y, 1998);
  f1_golden (f1_x2, f1_y2, 1998);
  f1_check (f1_x, f1_x2, f1_y, f1_y2, 1998);

  int32_t f2_x[2023], f2_x2[2023];
  int64_t f2_y[4046], f2_y2[4046];
  f2_init (f2_x, f2_x2, f2_y, f2_y2, 2023);
  f2 (f2_x, f2_y, 2023);
  f2_golden (f2_x2, f2_y2, 2023);
  f2_check (f2_x, f2_x2, f2_y, f2_y2, 2023);

  int8_t f3_x[3203], f3_x2[3203];
  int64_t f3_y[6406], f3_y2[6406];
  f3_init (f3_x, f3_x2, f3_y, f3_y2, 3203);
  f3 (f3_x, f3_y, 3203);
  f3_golden (f3_x2, f3_y2, 3203);
  f3_check (f3_x, f3_x2, f3_y, f3_y2, 3203);
  return 0;
}
