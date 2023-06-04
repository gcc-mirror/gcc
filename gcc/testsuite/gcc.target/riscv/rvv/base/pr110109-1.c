/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void __attribute__ ((noinline, noclone))
clean_subreg (int32_t *in, int32_t *out, size_t m)
{
   vint16m8_t v24, v8, v16;   
  vint32m8_t result = __riscv_vle32_v_i32m8 (in, 32);
  vint32m1_t v0 = __riscv_vget_v_i32m8_i32m1 (result, 0);
  vint32m1_t v1 = __riscv_vget_v_i32m8_i32m1 (result, 1);
  vint32m1_t v2 = __riscv_vget_v_i32m8_i32m1 (result, 2);
  vint32m1_t v3 = __riscv_vget_v_i32m8_i32m1 (result, 3);
  vint32m1_t v4 = __riscv_vget_v_i32m8_i32m1 (result, 4);
  vint32m1_t v5 = __riscv_vget_v_i32m8_i32m1 (result, 5);
  vint32m1_t v6 = __riscv_vget_v_i32m8_i32m1 (result, 6);
  vint32m1_t v7 = __riscv_vget_v_i32m8_i32m1 (result, 7);
  for (size_t i = 0; i < m; i++)
    {
      v0 = __riscv_vadd_vv_i32m1(v0, v0, 4);
      v1 = __riscv_vadd_vv_i32m1(v1, v1, 4);
      v2 = __riscv_vadd_vv_i32m1(v2, v2, 4);
      v3 = __riscv_vadd_vv_i32m1(v3, v3, 4);
      v4 = __riscv_vadd_vv_i32m1(v4, v4, 4);
      v5 = __riscv_vadd_vv_i32m1(v5, v5, 4);
      v6 = __riscv_vadd_vv_i32m1(v6, v6, 4);
      v7 = __riscv_vadd_vv_i32m1(v7, v7, 4);
    }
  vint32m8_t result2 = __riscv_vundefined_i32m8 ();
  result2 = __riscv_vset_v_i32m1_i32m8 (result2, 0, v0);
  result2 = __riscv_vset_v_i32m1_i32m8 (result2, 1, v1);
  result2 = __riscv_vset_v_i32m1_i32m8 (result2, 2, v2);
  result2 = __riscv_vset_v_i32m1_i32m8 (result2, 3, v3);
  result2 = __riscv_vset_v_i32m1_i32m8 (result2, 4, v4);
  result2 = __riscv_vset_v_i32m1_i32m8 (result2, 5, v5);
  result2 = __riscv_vset_v_i32m1_i32m8 (result2, 6, v6);
  result2 = __riscv_vset_v_i32m1_i32m8 (result2, 7, v7);
  __riscv_vse32_v_i32m8((out), result2, 64); 
}
