/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */

#include "riscv_vector.h"

void f (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vmv_v_v_i8mf4 (v, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f2 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vmv_v_x_i8mf4_tu (v, 3, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f3 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  vint8mf4_t v2 = __riscv_vle8_v_i8mf4 (base1 + 100, 32);
  vbool32_t m = __riscv_vlm_v_b32 (base1 + 200, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vmerge_vvm_i8mf4 (v, v2, m, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f4 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  vbool32_t m = __riscv_vlm_v_b32 (base1 + 200, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vmerge_vxm_i8mf4 (v, 3, m, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f5 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  vbool32_t m = __riscv_vlm_v_b32 (base1 + 200, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vmerge_vxm_i8mf4 (v, 100, m, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f6 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vuint8mf4_t v = __riscv_vle8_v_u8mf4 (base1 + 100, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vluxei8_v_u8mf4 (base2, v, 32);
    v = __riscv_vle8_v_u8mf4_tu (v, base2 + 200, 32);
  }
  __riscv_vse8_v_u8mf4 (out, v, 32);
}

void f7 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vadd_vv_i8mf4 (v, v, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f8 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vadd_vv_i8mf4 (v, v, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f9 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vsll_vx_i8mf4 (v, 101, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f10 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vor_vx_i8mf4 (v, 101, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f11 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  vbool32_t m = __riscv_vlm_v_b32 (base1 + 200, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vadc_vvm_i8mf4 (v, v, m, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f12 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  vbool32_t m = __riscv_vlm_v_b32 (base1 + 200, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vsbc_vvm_i8mf4 (v, v, m, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f13 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  vbool32_t m = __riscv_vlm_v_b32 (base1 + 200, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vadc_vxm_i8mf4 (v, 100, m, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f14 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  vbool32_t m = __riscv_vlm_v_b32 (base1 + 200, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vsbc_vxm_i8mf4 (v, 100, m, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f15 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vneg_v_i8mf4 (v, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f16 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vsadd_vv_i8mf4 (v, v, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f17 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vsadd_vx_i8mf4 (v, 100, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f18 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vaadd_vv_i8mf4 (v, v, 0, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

void f19 (int8_t* base1,int8_t* base2,int8_t* out,int n)
{
  vint8mf4_t v = __riscv_vle8_v_i8mf4 (base1, 32);
  for (int i = 0; i < n; i++){
    v = __riscv_vaadd_vx_i8mf4 (v, 100, 0, 32);
    v = __riscv_vle8_v_i8mf4_tu (v, base2, 32);
  }
  __riscv_vse8_v_i8mf4 (out, v, 32);
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
