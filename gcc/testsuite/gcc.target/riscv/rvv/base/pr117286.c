/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O1" } */

#include "riscv_vector.h"
_Float16 a[10];
void func(){
  int placeholder0 = 10;
  _Float16* ptr_a = a;
  for (size_t vl; placeholder0 > 0; placeholder0 -= vl){
    vl = __riscv_vsetvl_e16m1(placeholder0);
    vfloat16mf2_t va = __riscv_vle16_v_f16mf2(ptr_a, vl);
    vfloat16m1_t vb = __riscv_vlmul_ext_v_f16mf2_f16m1(va);
    vfloat16mf2_t vc = __riscv_vlmul_trunc_v_f16m1_f16mf2(vb);
    ptr_a += vl;
  }
}
