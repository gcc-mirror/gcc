/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

#define float32_t float

// Unit-Stride Load/Store
vint32m1_t test_vle32_v_i32m1_shortcut (size_t vl)
{
  return __riscv_vle32_v_i32m1 ((int32_t *)0, vl);
}

vuint32m1_t test_vle32_v_u32m1_shortcut (size_t vl)
{
  return __riscv_vle32_v_u32m1 ((int32_t *)0, vl);
}

vfloat32m1_t test_vle32_v_f32m1_shortcut (size_t vl)
{
  return __riscv_vle32_v_f32m1 ((float32_t *)0, vl);
}

void test_vse32_v_i32m1_shortcut (vint32m1_t val, size_t vl)
{
  __riscv_vse32_v_i32m1 ((int32_t *)0, val, vl);
}

void test_vse32_v_u32m1_shortcut (vuint32m1_t val, size_t vl)
{
  __riscv_vse32_v_u32m1 ((uint32_t *)0, val, vl);
}

void test_vse32_v_f32m1_shortcut (vfloat32m1_t val, size_t vl)
{
  __riscv_vse32_v_f32m1 ((float32_t *)0, val, vl);
}

// Stride Load/Store
vint32m1_t test_vlse32_v_i32m1_shortcut (ptrdiff_t bstride, size_t vl)
{
  return  __riscv_vlse32_v_i32m1 ((int32_t *)0, bstride, vl);
}

vuint32m1_t test_vlse32_v_u32m1_shortcut (ptrdiff_t bstride, size_t vl)
{
  return  __riscv_vlse32_v_u32m1 ((uint32_t *)0, bstride, vl);
}

vfloat32m1_t test_vlse32_v_f32m1_shortcut (ptrdiff_t bstride, size_t vl)
{
  return  __riscv_vlse32_v_f32m1 ((float32_t *)0, bstride, vl);
}

void test_vsse32_v_i32m1_shortcut (ptrdiff_t bstride, vint32m1_t val, size_t vl)
{
  __riscv_vsse32_v_i32m1 ((int32_t *)0, bstride, val, vl);
}

void test_vsse32_v_u32m1_shortcut (ptrdiff_t bstride, vuint32m1_t val, size_t vl)
{
  __riscv_vsse32_v_u32m1 ((uint32_t *)0, bstride, val, vl);
}

void test_vsse32_v_f32m1_shortcut (ptrdiff_t bstride, vfloat32m1_t val,size_t vl)
{
  __riscv_vsse32_v_f32m1 ((float32_t *)0, bstride, val, vl);
}

// Indexed-Unordered Load/Store
vint32m1_t test_vluxei32_v_i32m1_shortcut (vuint32m1_t bindex, size_t vl)
{
  return __riscv_vluxei32_v_i32m1 ((int32_t *)0, bindex, vl);
}

vuint32m1_t test_vluxei32_v_u32m1_shortcut (vuint32m1_t bindex, size_t vl)
{
  return __riscv_vluxei32_v_u32m1 ((uint32_t *)0, bindex, vl);
}

vfloat32m1_t test_vluxei32_v_f32m1_shortcut (vuint32m1_t bindex, size_t vl)
{
  return __riscv_vluxei32_v_f32m1 ((float32_t *)0, bindex, vl);
}

void test_vsuxei32_v_i32m1_shortcut (vuint32m1_t bindex, vint32m1_t val, size_t vl)
{
  __riscv_vsuxei32_v_i32m1 ((int32_t *)0, bindex, val, vl);
}

void test_vsuxei32_v_u32m1_shortcut (vuint32m1_t bindex, vuint32m1_t val, size_t vl)
{
  __riscv_vsuxei32_v_u32m1 ((uint32_t *)0, bindex, val, vl);
}

void test_vsuxei32_v_f32m1_shortcut (vuint32m1_t bindex, vfloat32m1_t val, size_t vl)
{
  __riscv_vsuxei32_v_f32m1 ((float32_t *)0, bindex, val, vl);
}

// Indexed-Ordered Load/Store
vint32m1_t test_vloxei32_v_i32m1_shortcut (vuint32m1_t bindex, size_t vl)
{
  return __riscv_vloxei32_v_i32m1 ((int32_t *)0, bindex, vl);
}

vuint32m1_t test_vloxei32_v_u32m1_shortcut (vuint32m1_t bindex, size_t vl)
{
  return __riscv_vloxei32_v_u32m1 ((uint32_t *)0, bindex, vl);
}

vfloat32m1_t test_vloxei32_v_f32m1_shortcut (vuint32m1_t bindex, size_t vl)
{
  return __riscv_vloxei32_v_f32m1 ((float32_t *)0, bindex, vl);
}

void test_vsoxei32_v_i32m1_shortcut (vuint32m1_t bindex, vint32m1_t val, size_t vl)
{
  __riscv_vsoxei32_v_i32m1 ((int32_t *)0, bindex, val, vl);
}

void test_vsoxei32_v_u32m1_shortcut (vuint32m1_t bindex, vuint32m1_t val, size_t vl)
{
  __riscv_vsoxei32_v_u32m1 ((uint32_t *)0, bindex, val, vl);
}

void test_vsoxei32_v_f32m1_shortcut (vuint32m1_t bindex, vfloat32m1_t val, size_t vl)
{
  __riscv_vsoxei32_v_f32m1 ((float32_t *)0, bindex, val, vl);
}

/* { dg-final { scan-assembler-times {v[ls]e[0-9]+\.v\s+v[0-9]+,\s*0\(zero\)} 6 } } */
/* { dg-final { scan-assembler-times {v[ls]se[0-9]+\.v\s+v[0-9]+,\s*0\(zero\),\s*[ax][0-9]+} 6 } } */
/* { dg-final { scan-assembler-times {v[ls][uo]xei[0-9]+\.v\s+v[0-9]+,\s*\(zero\),\s*v[0-9]+} 12 } } */
/* { dg-final { scan-assembler-not {li\s+[a-x][0-9]+,\s*0} } } */
