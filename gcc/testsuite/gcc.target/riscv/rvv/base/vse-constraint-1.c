/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

void f (int * restrict in, int * restrict out, void * restrict mask_in, int n)
{
  vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((float *)(in + 10000), 19);
  __riscv_vse32_v_f32mf2 ((float *)(out + 10000), v, 19);
  vbool64_t mask = *(vbool64_t*)mask_in;
  for (int i = 0; i < n; i++)
    {
      vint16mf2_t v1 = __riscv_vle16_v_i16mf2 ((int16_t *)(in + i + 1), 19);
      __riscv_vse16_v_i16mf2 ((int16_t *)(out + i + 1), v1, 19);

      vint32mf2_t v2 = __riscv_vle32_v_i32mf2 ((int32_t *)(in + i + 2), 19);
      __riscv_vse32_v_i32mf2 ((int32_t *)(out + i + 2), v2, 19);

      vint32mf2_t v3 = __riscv_vle32_v_i32mf2_tumu (mask, v2, (int32_t *)(in + i + 200), 13);
      __riscv_vse32_v_i32mf2 ((int32_t *)(out + i + 200), v3, 13);

      vfloat64m1_t v4 = __riscv_vle64_v_f64m1_m (mask, (double *)(in + i + 300), 11);
      __riscv_vse64_v_f64m1 ((double *)(out + i + 300), v4, 11);

      vfloat64m1_t v5 = __riscv_vle64_v_f64m1_tum (mask, v4, (double *)(in + i + 500), 11);
      __riscv_vse64_v_f64m1 ((double *)(out + i + 500), v5, 11);

      vfloat64m1_t v6 = __riscv_vle64_v_f64m1_mu (mask, v5, (double *)(in + i + 600), 11);
      __riscv_vse64_v_f64m1_m (mask, (double *)(out + i + 600), v6, 11);

      vuint8mf4_t v7 = __riscv_vle8_v_u8mf4 ((uint8_t *)(in + i + 700), 11);
      __riscv_vse8_v_u8mf4 ((uint8_t *)(out + i + 700), v7, 11);
    }
}

void f2 (int * restrict in, int * restrict out, void * restrict mask_in, int n)
{
  vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((float *)(in + 10000), 19);
  __riscv_vse32_v_f32mf2 ((float *)(out + 10000), v, 19);
  vbool64_t mask = *(vbool64_t*)mask_in;
  for (int i = 0; i < n; i++)
    {
      vint16mf2_t v1 = __riscv_vle16_v_i16mf2 ((int16_t *)(in + i + 1), 19);
      __riscv_vse16_v_i16mf2 ((int16_t *)(out + i + 1), v1, 19);

      vint32mf2_t v2 = __riscv_vle32_v_i32mf2 ((int32_t *)(in + i + 2), 19);
      __riscv_vse32_v_i32mf2 ((int32_t *)(out + i + 2), v2, 19);

      vint32mf2_t v3 = __riscv_vle32_v_i32mf2_tumu (mask, v2, (int32_t *)(in + i + 200), 13);
      __riscv_vse32_v_i32mf2 ((int32_t *)(out + i + 200), v2, 13);

      vfloat64m1_t v4 = __riscv_vle64_v_f64m1_m (mask, (double *)(in + i + 300), 11);
      __riscv_vse64_v_f64m1 ((double *)(out + i + 300), v4, 11);

      vfloat64m1_t v5 = __riscv_vle64_v_f64m1_tum (mask, v4, (double *)(in + i + 500), 11);
      __riscv_vse64_v_f64m1 ((double *)(out + i + 500), v5, 11);

      vfloat64m1_t v6 = __riscv_vle64_v_f64m1_mu (mask, v5, (double *)(in + i + 600), 11);
      __riscv_vse64_v_f64m1_m (mask, (double *)(out + i + 600), v6, 11);

      vuint8mf4_t v7 = __riscv_vle8_v_u8mf4 ((uint8_t *)(in + i + 700), 11);
      __riscv_vse8_v_u8mf4 ((uint8_t *)(out + i + 700), v7, 11);
    }
}

void f3 (int * restrict in, int * restrict out, void * restrict mask_in, int n)
{
  vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((float *)(in + 10000), 19);
  __riscv_vse32_v_f32mf2 ((float *)(out + 10000), v, 19);
  vbool64_t mask = *(vbool64_t*)mask_in;
  for (int i = 0; i < n; i++)
    {
      vint16mf2_t v1 = __riscv_vle16_v_i16mf2 ((int16_t *)(in + i + 1), 19);
      __riscv_vse16_v_i16mf2 ((int16_t *)(out + i + 1), v1, 19);

      vint32mf2_t v2 = __riscv_vle32_v_i32mf2 ((int32_t *)(in + i + 2), 19);
      __riscv_vse32_v_i32mf2 ((int32_t *)(out + i + 2), v2, 19);

      vint32mf2_t v3 = __riscv_vle32_v_i32mf2_tumu (mask, v2, (int32_t *)(in + i + 200), 13);
      *(vint32mf2_t*)(out + i + 200) = v3;

      vfloat64m1_t v4 = __riscv_vle64_v_f64m1_m (mask, (double *)(in + i + 300), 11);
      __riscv_vse64_v_f64m1 ((double *)(out + i + 300), v4, 11);

      vfloat64m1_t v5 = __riscv_vle64_v_f64m1_tum (mask, v4, (double *)(in + i + 500), 11);
      __riscv_vse64_v_f64m1 ((double *)(out + i + 500), v5, 11);

      vfloat64m1_t v6 = __riscv_vle64_v_f64m1_mu (mask, v5, (double *)(in + i + 600), 11);
      __riscv_vse64_v_f64m1_m (mask, (double *)(out + i + 600), v6, 11);

      vuint8mf4_t v7 = __riscv_vle8_v_u8mf4 ((uint8_t *)(in + i + 700), 11);
      __riscv_vse8_v_u8mf4 ((uint8_t *)(out + i + 700), v7, 11);
    }
}

/* It should not have redundant vector register spills which produce csrr vlenb instructions allocate stack.  */
/* { dg-final { scan-assembler-not {csrr\s+[a-x0-9]+,\s*vlenb} } } */
