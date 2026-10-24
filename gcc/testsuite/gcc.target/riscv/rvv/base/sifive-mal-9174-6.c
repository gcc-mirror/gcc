/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2 -mcpu=sifive-p870-d -mno-fix-sifive-mal-9191" } */

/* -mno-fix-sifive-mal-9191 overrides -mcpu.  */

#include <riscv_vector.h>

vint32m1_t
tama (int *base, vuint32m1_t index, size_t vl)
{
  return __riscv_vluxei32_v_i32m1 (base, index, vl);
}

vint32m1_t
tamu (vbool32_t m, int *base, vuint32m1_t index, size_t vl)
{
  return __riscv_vluxei32_v_i32m1_m (m, base, index, vl);
}

vint32m1_t
tu (int *base, vuint32m1_t index, size_t vl)
{
  vint32m1_t merge = __riscv_vreinterpret_v_u32m1_i32m1 (index);
  return __riscv_vluxei32_v_i32m1_tu (merge, base, index, vl);
}

vint32m1_t
tumu (vbool32_t m, int *base, vuint32m1_t index, size_t vl)
{
  vint32m1_t merge = __riscv_vreinterpret_v_u32m1_i32m1 (index);
  return __riscv_vluxei32_v_i32m1_tumu (m, merge, base, index, vl);
}

vint8mf8_t
se (void *base, vuint64m1_t index, size_t vl)
{
  return __riscv_vluxei64_v_i8mf8 (base, index, vl);
}

vint8mf8_t
se_tu (void *base, vuint64m1_t index, vint8mf8_t merge, size_t vl)
{
  return __riscv_vluxei64_v_i8mf8_tu (merge, base, index, vl);
}

/* { dg-final { scan-assembler {vl[ou]xei[0-9]+\.v\s+v([0-9]+),\([^)]*\),v\1\M} } } */
