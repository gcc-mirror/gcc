/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O1" } */
/* { dg-additional-sources abi-call-args-2.c } */

#include <stdlib.h>
#include <stdbool.h>
#include "riscv_vector.h"

int8_t
va_callee (int count, ...);

bool __attribute__ ((noinline)) va_caller ()
{
  size_t vlmax = __riscv_vsetvlmax_e8m1 ();
  vint8m1_t a1 = __riscv_vmv_v_x_i8m1 (1, vlmax);
  vint8m1_t a2 = __riscv_vmv_v_x_i8m1 (2, vlmax);
  vint8m1_t a3 = __riscv_vmv_v_x_i8m1 (3, vlmax);
  vint8m1_t a4 = __riscv_vmv_v_x_i8m1 (4, vlmax);
  vint8m1_t a5 = __riscv_vmv_v_x_i8m1 (5, vlmax);
  vint8m1_t a6 = __riscv_vmv_v_x_i8m1 (6, vlmax);
  vint8m1_t a7 = __riscv_vmv_v_x_i8m1 (7, vlmax);
  vint8m1_t a8 = __riscv_vmv_v_x_i8m1 (8, vlmax);
  int8_t sum = va_callee (8, a1, a2, a3, a4, a5, a6, a7, a8);

  return sum == (int8_t) vlmax * (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8);
}

int
main ()
{
  if (va_caller ())
    abort ();
  return 0;
}
