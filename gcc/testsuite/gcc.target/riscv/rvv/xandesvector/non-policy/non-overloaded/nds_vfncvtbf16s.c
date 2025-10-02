/* { dg-do compile } */
/* { dg-options "-march=rv32gv_xandesvbfhcvt -mabi=ilp32 -O3 -fno-schedule-insns -fno-schedule-insns2" { target { rv32 } } } */
/* { dg-options "-march=rv64gv_xandesvbfhcvt -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" { target { rv64 } } } */

#include "andes_vector.h"

vbfloat16mf4_t test_nds_vfncvt_bf16_s_bf16mf4(vfloat32mf2_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16mf4(vs2, vl);
}

vbfloat16mf2_t test_nds_vfncvt_bf16_s_bf16mf2(vfloat32m1_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16mf2(vs2, vl);
}

vbfloat16m1_t test_nds_vfncvt_bf16_s_bf16m1(vfloat32m2_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16m1(vs2, vl);
}

vbfloat16m2_t test_nds_vfncvt_bf16_s_bf16m2(vfloat32m4_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16m2(vs2, vl);
}

vbfloat16m4_t test_nds_vfncvt_bf16_s_bf16m4(vfloat32m8_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16m4(vs2, vl);
}

vbfloat16mf4_t test_nds_vfncvt_bf16_s_bf16mf4_rm(vfloat32mf2_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16mf4_rm(vs2, __RISCV_FRM_RNE, vl);
}

vbfloat16mf2_t test_nds_vfncvt_bf16_s_bf16mf2_rm(vfloat32m1_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16mf2_rm(vs2, __RISCV_FRM_RNE, vl);
}

vbfloat16m1_t test_nds_vfncvt_bf16_s_bf16m1_rm(vfloat32m2_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16m1_rm(vs2, __RISCV_FRM_RNE, vl);
}

vbfloat16m2_t test_nds_vfncvt_bf16_s_bf16m2_rm(vfloat32m4_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16m2_rm(vs2, __RISCV_FRM_RNE, vl);
}

vbfloat16m4_t test_nds_vfncvt_bf16_s_bf16m4_rm(vfloat32m8_t vs2, size_t vl) {
    return __riscv_nds_vfncvt_bf16_s_bf16m4_rm(vs2, __RISCV_FRM_RNE, vl);
}
/* { dg-final { scan-assembler-times {vseti?vli\s+[a-z0-9]+,\s*a0,\s*e[0-9]+,\s*mf?[1248],\s*t[au],\s*m[au]\s+vmv[1248]r\.v\s+[0-9v,]+\s+nds\.vfncvt\.bf16\.s[ivxfswum.]*\s+} 10 } } */
