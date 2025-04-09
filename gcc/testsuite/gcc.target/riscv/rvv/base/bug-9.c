/* Test that we do not have ice when compile */
/* { dg-do assemble } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2"  { target { rv64 } } } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O2"  { target { rv32 } } } */

#include "riscv_vector.h"

vfloat16m1_t f0 (vfloat16m1_t vs2, vfloat16m1_t vs1, size_t vl)
{
  return __riscv_vfadd_vv_f16m1_rm (vs2, vs1, 0, vl); 
}

/* { dg-error "return type 'vfloat16m1_t' requires the zvfhmin or zvfh ISA extension" "" { target { "riscv*-*-*" } } 0 } */
/* { dg-error "argument type 'vfloat16m1_t' requires the zvfhmin or zvfh ISA extension" "" { target { "riscv*-*-*" } } 0 } */
