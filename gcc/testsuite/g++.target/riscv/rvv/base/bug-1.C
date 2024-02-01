/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"
template < class T > 
bool __attribute__(()) check(T *, T *, size_t ); 
int main() { 
 size_t var_44 = 132u; 
 float var_43[41]; 
 int16_t var_32[41]; 
 int16_t var_31[41]; 
 float var_30[41]; 
 float var_29[41]; 
 float var_28[41]; 
 float var_27[41]; 
 float var_26[41]; 
 float var_25[41]; 
 float var_23 = (2732844647u); 
 int16_t var_22 = 23867; 
 vint16m4_t var_14 = __riscv_vle16_v_i16m4(var_32, 41); 
 vint16m4_t var_15 = __riscv_vle16_v_i16m4(var_31, 41); 
 vfloat32m8_t var_16 = __riscv_vle32_v_f32m8(var_30, 33); 
 vfloat32m8_t var_17 = __riscv_vle32_v_f32m8(var_29, 33); 
 vfloat32m8_t var_18 = __riscv_vle32_v_f32m8(var_28, 33); 
 vfloat32m8_t var_19 = __riscv_vle32_v_f32m8(var_27, 41); 
 vfloat32m8_t var_20 = __riscv_vle32_v_f32m8(var_26, 41); 
 vint16m4_t var_8 = __riscv_vmin_vv_i16m4(var_14, var_15, 41); 
 vfloat32m8_t var_7 = __riscv_vfmsac_vv_f32m8(var_16, var_17, var_18, 33); 
 vbool4_t var_6 = __riscv_vmsle_vx_i16m4_b4(var_8, var_22, 41); 
 float var_5 = __riscv_vfmv_f_s_f32m8_f32(var_7); 
 vfloat32m8_t var_4 = __riscv_vfnmsac_vf_f32m8_m(var_6, var_19, var_23, var_20, 41); 
 vfloat32m8_t var_0 = __riscv_vmerge_vvm_f32m8(var_4, var_4, var_6,41); 
 vfloat32m8_t var_1 = __riscv_vfmsub_vf_f32m8(var_0, var_5, var_4, 33); 
 __riscv_vse32_v_f32m8(var_25, var_1, 33); 
 if (!check(var_25, var_43, var_44)) 
   ; 
 return 0; 
}

/* { dg-final { scan-assembler-not {vmv} } } */
