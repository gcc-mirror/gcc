/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

int byte_mac_vec(unsigned char *a, unsigned char *b, int len) {
  size_t vlmax = __riscv_vsetvlmax_e8m1();
  vint32m4_t vec_s = __riscv_vmv_v_x_i32m4(0, vlmax);
  vint32m1_t vec_zero = __riscv_vmv_v_x_i32m1(0, vlmax);
  int k = len;

  for (size_t vl; k > 0; k -= vl, a += vl, b += vl) {
      vl = __riscv_vsetvl_e8m1(k);

      vuint8m1_t a8s = __riscv_vle8_v_u8m1(a, vl);
      vuint8m1_t b8s = __riscv_vle8_v_u8m1(b, vl);
      vuint32m4_t a8s_extended = __riscv_vzext_vf4_u32m4(a8s, vl);
      vuint32m4_t b8s_extended = __riscv_vzext_vf4_u32m4(a8s, vl);

      vint32m4_t a8s_as_i32 = __riscv_vreinterpret_v_u32m4_i32m4(a8s_extended);
      vint32m4_t b8s_as_i32 = __riscv_vreinterpret_v_u32m4_i32m4(b8s_extended);

      vec_s = __riscv_vmacc_vv_i32m4_tu(vec_s, a8s_as_i32, b8s_as_i32, vl);
  }

  vint32m1_t vec_sum = __riscv_vredsum_vs_i32m4_i32m1(vec_s, vec_zero, __riscv_vsetvl_e32m4(len));
  int sum = __riscv_vmv_x_s_i32m1_i32(vec_sum);

  return sum;
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 4 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
