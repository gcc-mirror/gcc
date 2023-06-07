/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */

#include "riscv_vector.h"

typedef _Float16 float16_t;

vfloat16mf4_t test_vfadd_vv_f16mf4(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfadd_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfadd_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfadd_vf_f16m8(op1, op2, vl);
}

vfloat16mf4_t test_vfsub_vv_f16mf4(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfsub_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfsub_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfsub_vf_f16m8(op1, op2, vl);
}

vfloat16mf4_t test_vfrsub_vf_f16mf4(vfloat16mf4_t op1, float16_t op2, size_t vl) {
  return __riscv_vfrsub_vf_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfrsub_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfrsub_vf_f16m8(op1, op2, vl);
}

vfloat32mf2_t test_vfwadd_vv_f32mf2(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfwadd_vv_f32mf2(op1, op2, vl);
}

vfloat32m8_t test_vfwadd_vv_f32m8(vfloat16m4_t op1, vfloat16m4_t op2, size_t vl) {
  return __riscv_vfwadd_vv_f32m8(op1, op2, vl);
}

vfloat32mf2_t test_vfwadd_wv_f32mf2(vfloat32mf2_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfwadd_wv_f32mf2(op1, op2, vl);
}

vfloat32m8_t test_vfwadd_wv_f32m8(vfloat32m8_t op1, vfloat16m4_t op2, size_t vl) {
  return __riscv_vfwadd_wv_f32m8(op1, op2, vl);
}

vfloat32mf2_t test_vfwsub_vv_f32mf2(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfwsub_vv_f32mf2(op1, op2, vl);
}

vfloat32m8_t test_vfwsub_vv_f32m8(vfloat16m4_t op1, vfloat16m4_t op2, size_t vl) {
  return __riscv_vfwsub_vv_f32m8(op1, op2, vl);
}

vfloat32mf2_t test_vfwsub_wv_f32mf2(vfloat32mf2_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfwsub_wv_f32mf2(op1, op2, vl);
}

vfloat32m8_t test_vfwsub_wv_f32m8(vfloat32m8_t op1, vfloat16m4_t op2, size_t vl) {
  return __riscv_vfwsub_wv_f32m8(op1, op2, vl);
}

vfloat16mf4_t test_vfmul_vv_f16mf4(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfmul_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfmul_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfmul_vf_f16m8(op1, op2, vl);
}

vfloat16mf4_t test_vfdiv_vv_f16mf4(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfdiv_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfdiv_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfdiv_vf_f16m8(op1, op2, vl);
}

vfloat16mf4_t test_vfrdiv_vf_f16mf4(vfloat16mf4_t op1, float16_t op2, size_t vl) {
  return __riscv_vfrdiv_vf_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfrdiv_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfrdiv_vf_f16m8(op1, op2, vl);
}

vfloat32mf2_t test_vfwmul_vv_f32mf2(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfwmul_vv_f32mf2(op1, op2, vl);
}

vfloat32m8_t test_vfwmul_vf_f32m8(vfloat16m4_t op1, float16_t op2, size_t vl) {
  return __riscv_vfwmul_vf_f32m8(op1, op2, vl);
}

vfloat16mf4_t test_vfmacc_vv_f16mf4(vfloat16mf4_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfmacc_vv_f16mf4(vd, vs1, vs2, vl);
}

vfloat16m8_t test_vfmacc_vf_f16m8(vfloat16m8_t vd, float16_t rs1, vfloat16m8_t vs2, size_t vl) {
  return __riscv_vfmacc_vf_f16m8(vd, rs1, vs2, vl);
}

vfloat16mf4_t test_vfnmacc_vv_f16mf4(vfloat16mf4_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfnmacc_vv_f16mf4(vd, vs1, vs2, vl);
}

vfloat16m8_t test_vfnmacc_vf_f16m8(vfloat16m8_t vd, float16_t rs1, vfloat16m8_t vs2, size_t vl) {
  return __riscv_vfnmacc_vf_f16m8(vd, rs1, vs2, vl);
}

vfloat16mf4_t test_vfmsac_vv_f16mf4(vfloat16mf4_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfmsac_vv_f16mf4(vd, vs1, vs2, vl);
}

vfloat16m8_t test_vfmsac_vf_f16m8(vfloat16m8_t vd, float16_t rs1, vfloat16m8_t vs2, size_t vl) {
  return __riscv_vfmsac_vf_f16m8(vd, rs1, vs2, vl);
}

vfloat16mf4_t test_vfnmsac_vv_f16mf4(vfloat16mf4_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfnmsac_vv_f16mf4(vd, vs1, vs2, vl);
}

vfloat16m8_t test_vfnmsac_vf_f16m8(vfloat16m8_t vd, float16_t rs1, vfloat16m8_t vs2, size_t vl) {
  return __riscv_vfnmsac_vf_f16m8(vd, rs1, vs2, vl);
}

vfloat16mf4_t test_vfmadd_vv_f16mf4(vfloat16mf4_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfmadd_vv_f16mf4(vd, vs1, vs2, vl);
}

vfloat16m8_t test_vfmadd_vf_f16m8(vfloat16m8_t vd, float16_t rs1, vfloat16m8_t vs2, size_t vl) {
  return __riscv_vfmadd_vf_f16m8(vd, rs1, vs2, vl);
}

vfloat16mf4_t test_vfnmadd_vv_f16mf4(vfloat16mf4_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfnmadd_vv_f16mf4(vd, vs1, vs2, vl);
}

vfloat16m8_t test_vfnmadd_vf_f16m8(vfloat16m8_t vd, float16_t rs1, vfloat16m8_t vs2, size_t vl) {
  return __riscv_vfnmadd_vf_f16m8(vd, rs1, vs2, vl);
}

vfloat16mf4_t test_vfmsub_vv_f16mf4(vfloat16mf4_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfmsub_vv_f16mf4(vd, vs1, vs2, vl);
}

vfloat16m8_t test_vfmsub_vf_f16m8(vfloat16m8_t vd, float16_t rs1, vfloat16m8_t vs2, size_t vl) {
  return __riscv_vfmsub_vf_f16m8(vd, rs1, vs2, vl);
}

vfloat16mf4_t test_vfnmsub_vv_f16mf4(vfloat16mf4_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfnmsub_vv_f16mf4(vd, vs1, vs2, vl);
}

vfloat16m8_t test_vfnmsub_vf_f16m8(vfloat16m8_t vd, float16_t rs1, vfloat16m8_t vs2, size_t vl) {
  return __riscv_vfnmsub_vf_f16m8(vd, rs1, vs2, vl);
}

vfloat32mf2_t test_vfwmacc_vv_f32mf2(vfloat32mf2_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfwmacc_vv_f32mf2(vd, vs1, vs2, vl);
}

vfloat32m8_t test_vfwmacc_vf_f32m8(vfloat32m8_t vd, float16_t vs1, vfloat16m4_t vs2, size_t vl) {
  return __riscv_vfwmacc_vf_f32m8(vd, vs1, vs2, vl);
}

vfloat32mf2_t test_vfwnmacc_vv_f32mf2(vfloat32mf2_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfwnmacc_vv_f32mf2(vd, vs1, vs2, vl);
}

vfloat32m8_t test_vfwnmacc_vf_f32m8(vfloat32m8_t vd, float16_t vs1, vfloat16m4_t vs2, size_t vl) {
  return __riscv_vfwnmacc_vf_f32m8(vd, vs1, vs2, vl);
}

vfloat32mf2_t test_vfwmsac_vv_f32mf2(vfloat32mf2_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfwmsac_vv_f32mf2(vd, vs1, vs2, vl);
}

vfloat32m8_t test_vfwmsac_vf_f32m8(vfloat32m8_t vd, float16_t vs1, vfloat16m4_t vs2, size_t vl) {
  return __riscv_vfwmsac_vf_f32m8(vd, vs1, vs2, vl);
}

vfloat32mf2_t test_vfwnmsac_vv_f32mf2(vfloat32mf2_t vd, vfloat16mf4_t vs1, vfloat16mf4_t vs2, size_t vl) {
  return __riscv_vfwnmsac_vv_f32mf2(vd, vs1, vs2, vl);
}

vfloat32m8_t test_vfwnmsac_vf_f32m8(vfloat32m8_t vd, float16_t vs1, vfloat16m4_t vs2, size_t vl) {
  return __riscv_vfwnmsac_vf_f32m8(vd, vs1, vs2, vl);
}

vfloat16mf4_t test_vfsqrt_v_f16mf4(vfloat16mf4_t op1, size_t vl) {
  return __riscv_vfsqrt_v_f16mf4(op1, vl);
}

vfloat16m8_t test_vfsqrt_v_f16m8(vfloat16m8_t op1, size_t vl) {
  return __riscv_vfsqrt_v_f16m8(op1, vl);
}

vfloat16mf4_t test_vfrsqrt7_v_f16mf4(vfloat16mf4_t op1, size_t vl) {
  return __riscv_vfrsqrt7_v_f16mf4(op1, vl);
}

vfloat16m8_t test_vfrsqrt7_v_f16m8(vfloat16m8_t op1, size_t vl) {
  return __riscv_vfrsqrt7_v_f16m8(op1, vl);
}

vfloat16mf4_t test_vfrec7_v_f16mf4(vfloat16mf4_t op1, size_t vl) {
  return __riscv_vfrec7_v_f16mf4(op1, vl);
}

vfloat16m8_t test_vfrec7_v_f16m8(vfloat16m8_t op1, size_t vl) {
  return __riscv_vfrec7_v_f16m8(op1, vl);
}

vfloat16mf4_t test_vfmin_vv_f16mf4(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfmin_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfmin_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfmin_vf_f16m8(op1, op2, vl);
}

vfloat16mf4_t test_vfmax_vv_f16mf4(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfmax_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfmax_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfmax_vf_f16m8(op1, op2, vl);
}

vfloat16mf4_t test_vfsgnj_vv_f16mf4(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfsgnj_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfsgnj_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfsgnj_vf_f16m8(op1, op2, vl);
}

vfloat16mf4_t test_vfsgnjn_vv_f16mf4(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfsgnjn_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfsgnjn_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfsgnjn_vf_f16m8(op1, op2, vl);
}

vfloat16mf4_t test_vfsgnjx_vv_f16mf4(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vfsgnjx_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vfsgnjx_vf_f16m8(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vfsgnjx_vf_f16m8(op1, op2, vl);
}

vbool64_t test_vmfeq_vv_f16mf4_b64(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vmfeq_vv_f16mf4_b64(op1, op2, vl);
}

vbool2_t test_vmfeq_vf_f16m8_b2(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vmfeq_vf_f16m8_b2(op1, op2, vl);
}

vbool64_t test_vmfne_vv_f16mf4_b64(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vmfne_vv_f16mf4_b64(op1, op2, vl);
}

vbool2_t test_vmfne_vf_f16m8_b2(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vmfne_vf_f16m8_b2(op1, op2, vl);
}

vbool64_t test_vmflt_vv_f16mf4_b64(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vmflt_vv_f16mf4_b64(op1, op2, vl);
}

vbool2_t test_vmflt_vf_f16m8_b2(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vmflt_vf_f16m8_b2(op1, op2, vl);
}

vbool64_t test_vmfle_vv_f16mf4_b64(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vmfle_vv_f16mf4_b64(op1, op2, vl);
}

vbool2_t test_vmfle_vf_f16m8_b2(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vmfle_vf_f16m8_b2(op1, op2, vl);
}

vbool64_t test_vmfgt_vv_f16mf4_b64(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vmfgt_vv_f16mf4_b64(op1, op2, vl);
}

vbool2_t test_vmfgt_vf_f16m8_b2(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vmfgt_vf_f16m8_b2(op1, op2, vl);
}

vbool64_t test_vmfge_vv_f16mf4_b64(vfloat16mf4_t op1, vfloat16mf4_t op2, size_t vl) {
  return __riscv_vmfge_vv_f16mf4_b64(op1, op2, vl);
}

vbool2_t test_vmfge_vf_f16m8_b2(vfloat16m8_t op1, float16_t op2, size_t vl) {
  return __riscv_vmfge_vf_f16m8_b2(op1, op2, vl);
}

vuint16mf4_t test_vfclass_v_u16mf4(vfloat16mf4_t op1, size_t vl) {
  return __riscv_vfclass_v_u16mf4(op1, vl);
}

vuint16m8_t test_vfclass_v_u16m8(vfloat16m8_t op1, size_t vl) {
  return __riscv_vfclass_v_u16m8(op1, vl);
}

vfloat16mf4_t test_vfmerge_vfm_f16mf4(vfloat16mf4_t op1, float16_t op2, vbool64_t mask, size_t vl) {
  return __riscv_vfmerge_vfm_f16mf4(op1, op2, mask, vl);
}

vfloat16m8_t test_vfmerge_vfm_f16m8(vfloat16m8_t op1, float16_t op2, vbool2_t mask, size_t vl) {
  return __riscv_vfmerge_vfm_f16m8(op1, op2, mask, vl);
}

vfloat16mf4_t test_vfmv_v_f_f16mf4(float16_t src, size_t vl) {
  return __riscv_vfmv_v_f_f16mf4(src, vl);
}

vfloat16m8_t test_vfmv_v_f_f16m8(float16_t src, size_t vl) {
  return __riscv_vfmv_v_f_f16m8(src, vl);
}

vint16mf4_t test_vfcvt_x_f_v_i16mf4(vfloat16mf4_t src, size_t vl) {
  return __riscv_vfcvt_x_f_v_i16mf4(src, vl);
}

vuint16m8_t test_vfcvt_xu_f_v_u16m8(vfloat16m8_t src, size_t vl) {
  return __riscv_vfcvt_xu_f_v_u16m8(src, vl);
}

vfloat16mf4_t test_vfcvt_f_x_v_f16mf4(vint16mf4_t src, size_t vl) {
  return __riscv_vfcvt_f_x_v_f16mf4(src, vl);
}

vfloat16m8_t test_vfcvt_f_xu_v_f16m8(vuint16m8_t src, size_t vl) {
  return __riscv_vfcvt_f_xu_v_f16m8(src, vl);
}

vint16mf4_t test_vfcvt_rtz_x_f_v_i16mf4(vfloat16mf4_t src, size_t vl) {
  return __riscv_vfcvt_rtz_x_f_v_i16mf4(src, vl);
}

vuint16m8_t test_vfcvt_rtz_xu_f_v_u16m8(vfloat16m8_t src, size_t vl) {
  return __riscv_vfcvt_rtz_xu_f_v_u16m8(src, vl);
}

vfloat16mf4_t test_vfwcvt_f_x_v_f16mf4(vint8mf8_t src, size_t vl) {
  return __riscv_vfwcvt_f_x_v_f16mf4(src, vl);
}

vuint32m8_t test_vfwcvt_xu_f_v_u32m8(vfloat16m4_t src, size_t vl) {
  return __riscv_vfwcvt_xu_f_v_u32m8(src, vl);
}

vint8mf8_t test_vfncvt_x_f_w_i8mf8(vfloat16mf4_t src, size_t vl) {
  return __riscv_vfncvt_x_f_w_i8mf8(src, vl);
}

vfloat16m4_t test_vfncvt_f_xu_w_f16m4(vuint32m8_t src, size_t vl) {
  return __riscv_vfncvt_f_xu_w_f16m4(src, vl);
}

vfloat16m1_t test_vfredosum_vs_f16mf4_f16m1(vfloat16mf4_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredosum_vs_f16mf4_f16m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredosum_vs_f16m8_f16m1(vfloat16m8_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredosum_vs_f16m8_f16m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredusum_vs_f16mf4_f16m1(vfloat16mf4_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredusum_vs_f16mf4_f16m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredusum_vs_f16m8_f16m1(vfloat16m8_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredusum_vs_f16m8_f16m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredmax_vs_f16mf4_f16m1(vfloat16mf4_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredmax_vs_f16mf4_f16m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredmax_vs_f16m8_f16m1(vfloat16m8_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredmax_vs_f16m8_f16m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredmin_vs_f16mf4_f16m1(vfloat16mf4_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredmin_vs_f16mf4_f16m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredmin_vs_f16m8_f16m1(vfloat16m8_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredmin_vs_f16m8_f16m1(vector, scalar, vl);
}

vfloat32m1_t test_vfwredosum_vs_f16mf4_f32m1(vfloat16mf4_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfwredosum_vs_f16mf4_f32m1(vector, scalar, vl);
}

vfloat32m1_t test_vfwredosum_vs_f16m8_f32m1(vfloat16m8_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfwredosum_vs_f16m8_f32m1(vector, scalar, vl);
}

vfloat32m1_t test_vfwredusum_vs_f16mf4_f32m1(vfloat16mf4_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfwredusum_vs_f16mf4_f32m1(vector, scalar, vl);
}

vfloat32m1_t test_vfwredusum_vs_f16m8_f32m1(vfloat16m8_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfwredusum_vs_f16m8_f32m1(vector, scalar, vl);
}

vfloat16mf4_t test_vfslide1up_vf_f16mf4(vfloat16mf4_t src, float16_t value, size_t vl) {
  return __riscv_vfslide1up_vf_f16mf4(src, value, vl);
}

vfloat16m8_t test_vfslide1up_vf_f16m8(vfloat16m8_t src, float16_t value, size_t vl) {
  return __riscv_vfslide1up_vf_f16m8(src, value, vl);
}

vfloat16mf4_t test_vfslide1down_vf_f16mf4(vfloat16mf4_t src, float16_t value, size_t vl) {
  return __riscv_vfslide1down_vf_f16mf4(src, value, vl);
}

vfloat16m8_t test_vfslide1down_vf_f16m8(vfloat16m8_t src, float16_t value, size_t vl) {
  return __riscv_vfslide1down_vf_f16m8(src, value, vl);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 51 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]} 11 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]} 42 } } */
/* { dg-final { scan-assembler-times {vfadd\.v[fv]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfsub\.v[fv]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfrsub\.vf\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfwadd\.[wv]v\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vfwsub\.[wv]v\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vfmul\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfdiv\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfrdiv\.vf\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfwmul\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfmacc\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfnmacc\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfmsac\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfnmsac\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfmadd\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfnmadd\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfmsub\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfnmsub\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfwmacc\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfwnmacc\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfwmsac\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfwnmsac\.v[vf]\s+v[0-9]+,\s*[vfa]+[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfsqrt\.v\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfrsqrt7\.v\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfrec7\.v\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfmin\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfmax\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfsgnj\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfsgnjn\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfsgnjx\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmfeq\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmfne\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmflt\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmfle\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmfgt\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmfge\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[vfa]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfclass\.v\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfmerge\.vfm\s+v[0-9]+,\s*v[0-9]+,\s*fa[0-9]+,\s*v0} 2 } } */
/* { dg-final { scan-assembler-times {vfmv\.v\.f\s+v[0-9]+,\s*fa[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfcvt\.xu\.f\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfcvt\.f\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfcvt\.f\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfcvt\.rtz\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfcvt\.rtz\.xu\.f\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.xu\.f\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfncvt\.x\.f\.w\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfncvt\.f\.xu\.w\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vfredosum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfredusum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfredmax\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfredmin\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfwredosum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfwredusum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfslide1up\.vf\s+v[0-9]+,\s*v[0-9]+,\s*fa[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfslide1down\.vf\s+v[0-9]+,\s*v[0-9]+,\s*fa[0-9]+} 2 } } */
