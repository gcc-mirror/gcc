/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" } */

#include "riscv_vector.h"

void f0 (void * in, void *out, int64_t x, int n)
{
  
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmadc_vx_i64m1_b64 (v2, -16, 4);
  vbool64_t v4 = __riscv_vmadc_vx_i64m1_b64 (v, -16, 4);
  __riscv_vsm_v_b64 (out + 200, v3, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

void f1 (void * in, void *out, int64_t x, int n)
{
  
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmadc_vx_i64m1_b64 (v2, 15, 4);
  vbool64_t v4 = __riscv_vmadc_vx_i64m1_b64 (v, 15, 4);
  __riscv_vsm_v_b64 (out + 200, v3, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

void f2 (void * in, void *out, int64_t x, int n)
{
  
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmadc_vx_i64m1_b64 (v2, -17, 4);
  vbool64_t v4 = __riscv_vmadc_vx_i64m1_b64 (v, -17, 4);
  __riscv_vsm_v_b64 (out + 200, v3, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

void f3 (void * in, void *out, int64_t x, int n)
{
  
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmadc_vx_i64m1_b64 (v2, 16, 4);
  vbool64_t v4 = __riscv_vmadc_vx_i64m1_b64 (v, 16, 4);
  __riscv_vsm_v_b64 (out + 200, v3, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

void f4 (void * in, void *out, int64_t x, int n)
{
  
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmadc_vx_i64m1_b64 (v2, 0xAAAAAAA, 4);
  vbool64_t v4 = __riscv_vmadc_vx_i64m1_b64 (v, 0xAAAAAAA, 4);
  __riscv_vsm_v_b64 (out + 200, v3, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

void f5 (void * in, void *out, int64_t x, int n)
{
  
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmadc_vx_i64m1_b64 (v2, 0xAAAAAAAAAAAAAAAA, 4);
  vbool64_t v4 = __riscv_vmadc_vx_i64m1_b64 (v, 0xAAAAAAAAAAAAAAAA, 4);
  __riscv_vsm_v_b64 (out + 200, v3, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

void f6 (void * in, void *out, int64_t x, int n)
{
  
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmadc_vx_i64m1_b64 (v2, x, 4);
  vbool64_t v4 = __riscv_vmadc_vx_i64m1_b64 (v, x, 4);
  __riscv_vsm_v_b64 (out + 200, v3, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

/* { dg-final { scan-assembler-times {vmadc\.vi\s+v[0-9]+,\s*v[0-9]+,\s*-16} 2 } } */
/* { dg-final { scan-assembler-times {vmadc\.vi\s+v[0-9]+,\s*v[0-9]+,\s*15} 2 } } */
/* { dg-final { scan-assembler-times {vmadc\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 6 } } */
/* { dg-final { scan-assembler-times {vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
