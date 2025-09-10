/* { dg-do compile } */
/* { dg-options "-march=rv32gv_xandesvbfhcvt -mabi=ilp32 -O3 -fno-schedule-insns -fno-schedule-insns2" { target { rv32 } } } */
/* { dg-options "-march=rv64gv_xandesvbfhcvt -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" { target { rv64 } } } */

#include "andes_vector.h"

vfloat32mf2_t test_nds_vfwcvt_s_bf16_f32mf2(vbfloat16mf4_t vs2, size_t vl) {
    return __riscv_nds_vfwcvt_s_bf16_f32mf2(vs2, vl);
}

vfloat32m1_t test_nds_vfwcvt_s_bf16_f32m1(vbfloat16mf2_t vs2, size_t vl) {
    return __riscv_nds_vfwcvt_s_bf16_f32m1(vs2, vl);
}

vfloat32m2_t test_nds_vfwcvt_s_bf16_f32m2(vbfloat16m1_t vs2, size_t vl) {
    return __riscv_nds_vfwcvt_s_bf16_f32m2(vs2, vl);
}

vfloat32m4_t test_nds_vfwcvt_s_bf16_f32m4(vbfloat16m2_t vs2, size_t vl) {
    return __riscv_nds_vfwcvt_s_bf16_f32m4(vs2, vl);
}

vfloat32m8_t test_nds_vfwcvt_s_bf16_f32m8(vbfloat16m4_t vs2, size_t vl) {
    return __riscv_nds_vfwcvt_s_bf16_f32m8(vs2, vl);
}
/* { dg-final { scan-assembler-times {vseti?vli\s+[a-z0-9]+,\s*a0,\s*e[0-9]+,\s*mf?[1248],\s*t[au],\s*m[au]\s+vmv[1248]r\.v\s+[0-9v,]+\s+nds\.vfwcvt\.s\.bf16[ivxfswum.]*\s+} 5 } } */
