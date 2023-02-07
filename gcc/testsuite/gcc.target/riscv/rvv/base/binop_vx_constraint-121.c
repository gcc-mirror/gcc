/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" } */
#include "riscv_vector.h"

void f1 (void * in, void *out, int32_t x)
{
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
    vint32m1_t v3 = __riscv_vmulh_vx_i32m1 (v2, 0, 4);
    __riscv_vse32_v_i32m1 (out, v3, 4);
}

void f2 (void * in, void *out, int32_t x)
{
    vint64m1_t v = __riscv_vle64_v_i64m1 (in, 4);
    vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in, 4);
    vint64m1_t v3 = __riscv_vmulh_vx_i64m1 (v2, 0, 4);
    __riscv_vse64_v_i64m1 (out, v3, 4);
}

void f3 (void * in, void *out, int32_t x)
{
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
    vint32m1_t v3 = __riscv_vdiv_vx_i32m1 (v2, 0, 4);
    __riscv_vse32_v_i32m1 (out, v3, 4);
}

void f4 (void * in, void *out, int32_t x)
{
    vint64m1_t v = __riscv_vle64_v_i64m1 (in, 4);
    vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in, 4);
    vint64m1_t v3 = __riscv_vdiv_vx_i64m1 (v2, 0, 4);
    __riscv_vse64_v_i64m1 (out, v3, 4);
}

void f5 (void * in, void *out, int32_t x)
{
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
    vint32m1_t v3 = __riscv_vrem_vx_i32m1 (v2, 0, 4);
    __riscv_vse32_v_i32m1 (out, v3, 4);
}

void f6 (void * in, void *out, int32_t x)
{
    vint64m1_t v = __riscv_vle64_v_i64m1 (in, 4);
    vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in, 4);
    vint64m1_t v3 = __riscv_vrem_vx_i64m1 (v2, 0, 4);
    __riscv_vse64_v_i64m1 (out, v3, 4);
}

/* { dg-final { scan-assembler-times {vmulh\.vx\s+v[0-9]+,\s*v[0-9]+,zero} 2 } } */
/* { dg-final { scan-assembler-times {vdiv\.vx\s+v[0-9]+,\s*v[0-9]+,zero} 2 } } */
/* { dg-final { scan-assembler-times {vrem\.vx\s+v[0-9]+,\s*v[0-9]+,zero} 2 } } */
