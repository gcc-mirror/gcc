/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" } */

#include "riscv_vector.h"

void f0 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmerge_vxm_i64m1 (v2, -16, mask, 4);
  vint64m1_t v4 = __riscv_vmerge_vxm_i64m1 (v3, -16, mask, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

void f1 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmerge_vxm_i64m1 (v2, 15, mask, 4);
  vint64m1_t v4 = __riscv_vmerge_vxm_i64m1 (v3, 15, mask, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

void f2 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmerge_vxm_i64m1 (v2, -17, mask, 4);
  vint64m1_t v4 = __riscv_vmerge_vxm_i64m1 (v3, -17, mask, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

void f3 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmerge_vxm_i64m1 (v2, 16, mask, 4);
  vint64m1_t v4 = __riscv_vmerge_vxm_i64m1 (v3, 16, mask, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

void f4 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmerge_vxm_i64m1 (v2, 0xAAAAAAA, mask, 4);
  vint64m1_t v4 = __riscv_vmerge_vxm_i64m1 (v3, 0xAAAAAAA, mask, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

void f5 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmerge_vxm_i64m1 (v2, 0xAAAAAAAAAAAAAAAA, mask, 4);
  vint64m1_t v4 = __riscv_vmerge_vxm_i64m1 (v3, 0xAAAAAAAAAAAAAAAA, mask, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

void f6 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmerge_vxm_i64m1 (v2, x, mask, 4);
  vint64m1_t v4 = __riscv_vmerge_vxm_i64m1 (v3, x, mask, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/* { dg-final { scan-assembler-times {vmerge\.vim\s+v[0-9]+,\s*v[0-9]+,\s*-16,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmerge\.vim\s+v[0-9]+,\s*v[0-9]+,\s*15,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmerge\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 6 } } */
/* { dg-final { scan-assembler-times {vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
