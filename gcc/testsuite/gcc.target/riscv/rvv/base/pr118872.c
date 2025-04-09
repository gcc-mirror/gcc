/* Test that we do not have ice when compile */
/* { dg-do assemble } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2"  { target { rv64 } } } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O2"  { target { rv32 } } } */

#include "riscv_vector.h"

vfloat32m2_t foo (vfloat16m1_t a, size_t vl)
{
  return __riscv_vfwcvt_f_f_v_f32m2(a, vl);
}

/* { dg-error "argument type 'vfloat16m1_t' requires the zvfhmin or zvfh ISA extension" "" { target { "riscv*-*-*" } } 0 } */
