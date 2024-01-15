/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64x -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint64m1_t test_vslide1up_vx_i64m1_m(vbool64_t mask, vint64m1_t src, int64_t value, size_t vl) {
  return __riscv_vslide1up_vx_i64m1_m(mask, src, value, vl);
}

vint64m2_t test_vslide1up_vx_i64m2_m(vbool32_t mask, vint64m2_t src, int64_t value, size_t vl) {
  return __riscv_vslide1up_vx_i64m2_m(mask, src, value, vl);
}

vint64m4_t test_vslide1up_vx_i64m4_m(vbool16_t mask, vint64m4_t src, int64_t value, size_t vl) {
  return __riscv_vslide1up_vx_i64m4_m(mask, src, value, vl);
}

vint64m8_t test_vslide1up_vx_i64m8_m(vbool8_t mask, vint64m8_t src, int64_t value, size_t vl) {
  return __riscv_vslide1up_vx_i64m8_m(mask, src, value, vl);
}

/* { dg-final { scan-assembler-times {vseti?vli\s+[a-z0-9]+,\s*[a-z0-9]+,\s*e[0-9]+,\s*mf?[1248],\s*t[au],\s*m[au]\s+vslide1up\.[ivxfswum.]+\s+} 4 } } */
