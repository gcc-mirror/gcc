/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gc_zve64f_zvfh -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

void foo(_Float16 y, int16_t z, int64_t *i64p)
{
  vint64m1_t vx =__riscv_vle64_v_i64m1 (i64p, 1);
  vx = __riscv_vadd_vv_i64m1 (vx, vx, 1);
  vfloat16m1_t vy =__riscv_vfmv_s_f_f16m1 (y, 1);
  vint16m1_t vz =__riscv_vmv_s_x_i16m1 (z, 1);
  asm volatile ("# use %0 %1" : : "vr"(vx), "vr" (vy), "vr" (vz));
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*1,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e64,\s*m1,\s*t[au],\s*m[au]} 1 } } */
