/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

void f0 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfadd_vv_f32m1 (v, v, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

void f1 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfadd_vf_f32m1 (v, 8.9, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

void f2 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfdiv_vv_f32m1 (v, v, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

void f4 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfdiv_vf_f32m1 (v, 8.9, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

void f5 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfrdiv_vf_f32m1 (v, 8.9, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

void f6 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfsgnj_vv_f32m1 (v, v, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

void f7 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfsgnj_vf_f32m1 (v, 8.9, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

void f8 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfsqrt_v_f32m1 (v, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

void f9 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfrec7_v_f32m1 (v, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

void f10 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  vbool32_t m = __riscv_vlm_v_b32 (base1 + 200, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vmerge_vxm_i8mf4 (v, 100, m, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f11 (void* base1,void* base2,void* out,int n)
{
  vfloat32m1_t v = __riscv_vle32_v_f32m1 (base1, 32);
  vbool32_t m = __riscv_vlm_v_b32 (base1 + 200, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vfmerge_vfm_f32m1 (v, 8.9, m, 32);
    v = __riscv_vle32_v_f32m1_tu (v, base2, 32);
  }
  __riscv_vse32_v_f32m1 (out, v, 32);
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
