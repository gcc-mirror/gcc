/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-move-loop-invariants" } */

#include "riscv_vector.h"

void f (int * restrict in, int * restrict out, void * restrict mask_in, int n)
{
  vbool64_t mask = *(vbool64_t*)mask_in;
  for (int i = 0; i < n; i++)
    {
      vint8mf8_t v_8mf8_0 = __riscv_vle8_v_i8mf8 ((int8_t *)(in + i), 0);
      __riscv_vse8_v_i8mf8 ((int8_t *)(out + i), v_8mf8_0, 0);

      vint8mf8_t v_8mf8_0_tu = __riscv_vle8_v_i8mf8_tu (v_8mf8_0, (int8_t *)(in + i + 1), 0);
      __riscv_vse8_v_i8mf8_m (mask, (int8_t *)(out + i + 1), v_8mf8_0_tu, 0);

      vint8mf8_t v_8mf8_0_mu = __riscv_vle8_v_i8mf8_mu (mask, v_8mf8_0, (int8_t *)(in + i + 2), 0);
      __riscv_vse8_v_i8mf8 ((int8_t *)(out + i + 2), v_8mf8_0_tu, 0);

      vint8mf8_t v_8mf8_1 = __riscv_vle8_v_i8mf8 ((int8_t *)(in + i + 3), 7);
      __riscv_vse8_v_i8mf8 ((int8_t *)(out + i + 3), v_8mf8_1, 7);
      
      vint8mf8_t v_8mf8_2 = __riscv_vle8_v_i8mf8 ((int8_t *)(in + i + 4), 17);
      __riscv_vse8_v_i8mf8 ((int8_t *)(out + i + 4), v_8mf8_2, 17);

      vint8mf8_t v_8mf8_3 = __riscv_vle8_v_i8mf8 ((int8_t *)(in + i + 5), 27);
      __riscv_vse8_v_i8mf8 ((int8_t *)(out + i + 5), v_8mf8_3, 27);

      vint8mf4_t v_8mf4_1 = __riscv_vle8_v_i8mf4 ((int8_t *)(in + i + 6), 7);
      __riscv_vse8_v_i8mf4 ((int8_t *)(out + i + 6), v_8mf4_1, 7);
      
      vint8mf4_t v_8mf4_2 = __riscv_vle8_v_i8mf4 ((int8_t *)(in + i + 7), 17);
      __riscv_vse8_v_i8mf4 ((int8_t *)(out + i + 7), v_8mf4_2, 17);

      vint8mf4_t v_8mf4_3 = __riscv_vle8_v_i8mf4 ((int8_t *)(in + i + 8), 27);
      __riscv_vse8_v_i8mf4 ((int8_t *)(out + i + 8), v_8mf4_3, 27);

      vint8mf2_t v_8mf2_1 = __riscv_vle8_v_i8mf2 ((int8_t *)(in + i + 9), 7);
      __riscv_vse8_v_i8mf2 ((int8_t *)(out + i + 9), v_8mf2_1, 7);
      
      vint8mf2_t v_8mf2_2 = __riscv_vle8_v_i8mf2 ((int8_t *)(in + i + 10), 17);
      __riscv_vse8_v_i8mf2 ((int8_t *)(out + i + 10), v_8mf2_2, 17);

      vint8mf2_t v_8mf2_3 = __riscv_vle8_v_i8mf2 ((int8_t *)(in + i + 11), 27);
      __riscv_vse8_v_i8mf2 ((int8_t *)(out + i + 11), v_8mf2_3, 27);

      vint8m1_t v_8m1_1 = __riscv_vle8_v_i8m1 ((int8_t *)(in + i + 12), 7);
      __riscv_vse8_v_i8m1 ((int8_t *)(out + i + 12), v_8m1_1, 7);
      
      vint8m1_t v_8m1_2 = __riscv_vle8_v_i8m1 ((int8_t *)(in + i + 13), 17);
      __riscv_vse8_v_i8m1 ((int8_t *)(out + i + 13), v_8m1_2, 17);

      vint8m1_t v_8m1_3 = __riscv_vle8_v_i8m1 ((int8_t *)(in + i + 14), 27);
      __riscv_vse8_v_i8m1 ((int8_t *)(out + i + 14), v_8m1_3, 27);

      vint8m2_t v_8m2_1 = __riscv_vle8_v_i8m2 ((int8_t *)(in + i + 15), 7);
      __riscv_vse8_v_i8m2 ((int8_t *)(out + i + 15), v_8m2_1, 7);
      
      vint8m2_t v_8m2_2 = __riscv_vle8_v_i8m2 ((int8_t *)(in + i + 16), 17);
      __riscv_vse8_v_i8m2 ((int8_t *)(out + i + 16), v_8m2_2, 17);

      vint8m2_t v_8m2_3 = __riscv_vle8_v_i8m2 ((int8_t *)(in + i + 17), 27);
      __riscv_vse8_v_i8m2 ((int8_t *)(out + i + 17), v_8m2_3, 27);

      vint8m4_t v_8m4_1 = __riscv_vle8_v_i8m4 ((int8_t *)(in + i + 18), 7);
      __riscv_vse8_v_i8m4 ((int8_t *)(out + i + 18), v_8m4_1, 7);
      
      vint8m4_t v_8m4_2 = __riscv_vle8_v_i8m4 ((int8_t *)(in + i + 19), 17);
      __riscv_vse8_v_i8m4 ((int8_t *)(out + i + 19), v_8m4_2, 17);

      vint8m4_t v_8m4_3 = __riscv_vle8_v_i8m4 ((int8_t *)(in + i + 20), 27);
      __riscv_vse8_v_i8m4 ((int8_t *)(out + i + 20), v_8m4_3, 27);

      vint8m8_t v_8m8_1 = __riscv_vle8_v_i8m8 ((int8_t *)(in + i + 21), 7);
      __riscv_vse8_v_i8m8 ((int8_t *)(out + i + 21), v_8m8_1, 7);
      
      vint8m8_t v_8m8_2 = __riscv_vle8_v_i8m8 ((int8_t *)(in + i + 22), 17);
      __riscv_vse8_v_i8m8 ((int8_t *)(out + i + 22), v_8m8_2, 17);

      vint8m8_t v_8m8_3 = __riscv_vle8_v_i8m8 ((int8_t *)(in + i + 23), 27);
      __riscv_vse8_v_i8m8 ((int8_t *)(out + i + 23), v_8m8_3, 27);

      vuint16mf4_t v_16mf4_1 = *(vuint16mf4_t*)(in + 24 + i);
      *(vuint16mf4_t*)(out + 24 + i) = v_16mf4_1;
      
      vuint16mf2_t v_16mf2_1 = *(vuint16mf2_t*)(in + 25 + i);
      *(vuint16mf2_t*)(out + 25 + i) = v_16mf2_1;

      vuint32mf2_t v_32mf2_t = *(vuint32mf2_t*)(in + 26 + i);
      *(vuint32mf2_t*)(out + 26 + i) = v_32mf2_t;

      vuint8mf2_t v_8mf2_4 = *(vuint8mf2_t*)(in + 27 + i);
      *(vuint8mf2_t*)(out + 27 + i) = v_8mf2_4;

      vuint8mf4_t v_8mf4_4 = *(vuint8mf4_t*)(in + 28 + i);
      *(vuint8mf4_t*)(out + 28 + i) = v_8mf4_4;

      vint32mf2_t v_32mf2_1 = __riscv_vle32_v_i32mf2 ((int32_t *)(in + i + 49), 7);
      __riscv_vse32_v_i32mf2 ((int32_t *)(out + i + 49), v_32mf2_1, 7);
      
      vint32mf2_t v_32mf2_2 = __riscv_vle32_v_i32mf2 ((int32_t *)(in + i + 30), 17);
      __riscv_vse32_v_i32mf2 ((int32_t *)(out + i + 30), v_32mf2_2, 17);

      vint32mf2_t v_32mf2_3 = __riscv_vle32_v_i32mf2 ((int32_t *)(in + i + 31), 27);
      __riscv_vse32_v_i32mf2 ((int32_t *)(out + i + 31), v_32mf2_3, 27);

      vint32m1_t v_32m1_1 = __riscv_vle32_v_i32m1 ((int32_t *)(in + i + 32), 7);
      __riscv_vse32_v_i32m1 ((int32_t *)(out + i + 32), v_32m1_1, 7);
      
      vint32m1_t v_32m1_2 = __riscv_vle32_v_i32m1 ((int32_t *)(in + i + 33), 17);
      __riscv_vse32_v_i32m1 ((int32_t *)(out + i + 33), v_32m1_2, 17);

      vint32m1_t v_32m1_3 = __riscv_vle32_v_i32m1 ((int32_t *)(in + i + 34), 27);
      __riscv_vse32_v_i32m1 ((int32_t *)(out + i + 34), v_32m1_3, 27);

      vint32m2_t v_32m2_1 = __riscv_vle32_v_i32m2 ((int32_t *)(in + i + 35), 7);
      __riscv_vse32_v_i32m2 ((int32_t *)(out + i + 35), v_32m2_1, 7);
      
      vint32m2_t v_32m2_2 = __riscv_vle32_v_i32m2 ((int32_t *)(in + i + 36), 17);
      __riscv_vse32_v_i32m2 ((int32_t *)(out + i + 36), v_32m2_2, 17);

      vint32m2_t v_32m2_3 = __riscv_vle32_v_i32m2 ((int32_t *)(in + i + 37), 27);
      __riscv_vse32_v_i32m2 ((int32_t *)(out + i + 37), v_32m2_3, 27);

      vint32m4_t v_32m4_1 = __riscv_vle32_v_i32m4 ((int32_t *)(in + i + 38), 7);
      __riscv_vse32_v_i32m4 ((int32_t *)(out + i + 38), v_32m4_1, 7);
      
      vint32m4_t v_32m4_2 = __riscv_vle32_v_i32m4 ((int32_t *)(in + i + 39), 17);
      __riscv_vse32_v_i32m4 ((int32_t *)(out + i + 39), v_32m4_2, 17);

      vint32m4_t v_32m4_3 = __riscv_vle32_v_i32m4 ((int32_t *)(in + i + 40), 27);
      __riscv_vse32_v_i32m4 ((int32_t *)(out + i + 40), v_32m4_3, 27);

      vint32m8_t v_32m8_1 = __riscv_vle32_v_i32m8 ((int32_t *)(in + i + 41), 7);
      __riscv_vse32_v_i32m8 ((int32_t *)(out + i + 41), v_32m8_1, 7);
      
      vint32m8_t v_32m8_2 = __riscv_vle32_v_i32m8 ((int32_t *)(in + i + 42), 17);
      __riscv_vse32_v_i32m8 ((int32_t *)(out + i + 42), v_32m8_2, 17);

      vint32m8_t v_32m8_3 = __riscv_vle32_v_i32m8 ((int32_t *)(in + i + 43), 27);
      __riscv_vse32_v_i32m8 ((int32_t *)(out + i + 43), v_32m8_3, 27);
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*0,\s*e8,\s*mf8,\s*tu,\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e8,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e8,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e8,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e8,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e8,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e8,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e8,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e8,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e32,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e32,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e32,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e32,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*7,\s*e32,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*27,\s*e32,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 6 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli} 37 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
