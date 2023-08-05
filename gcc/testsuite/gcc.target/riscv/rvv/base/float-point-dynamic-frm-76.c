/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3 -Wno-psabi" } */

#include "riscv_vector.h"

extern size_t normalize_vl_1 ();
extern size_t normalize_vl_2 ();

void
test_float_point_dynamic_frm (vfloat32m1_t op1, vfloat32m1_t op2,
			      size_t vl, int cond)
{
  vfloat32m1_t result_1;

  asm volatile ("#def %0" : "=vr"(result_1));

  result_1 = __riscv_vfadd_vv_f32m1 (op1, op2, vl);

  asm volatile ("#use %0" : : "vr"(result_1));

  if (cond)
    normalize_vl_1 ();
  else
    normalize_vl_2 ();

  vfloat32m1_t result_2;

  asm volatile ("#def %0" : "=vr"(result_2));

  result_2 = __riscv_vfadd_vv_f32m1 (op1, op2, vl);

  asm volatile ("#use %0" : : "vr"(result_2));
}

/* { dg-final { scan-assembler-times {vfadd\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[fav]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-not {frrm\s+[axs][0-9]+} } } */
/* { dg-final { scan-assembler-not {fsrm\s+[axs][0-9]+} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[01234]} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[axs][0-9]+,\s*[01234]} } } */
