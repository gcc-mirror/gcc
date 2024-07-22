/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vfloat32mf2_t test_vfwadd_wf_f32mf2(vfloat32mf2_t vs2, _Float16 rs1, size_t vl)
{
  return __riscv_vfwadd_wf_f32mf2(vs2, rs1, vl); /* { dg-error {built-in function '__riscv_vfwadd_wf_f32mf2\(vs2,  rs1,  vl\)' requires the zvfhmin or zvfh ISA extension} } */
}
