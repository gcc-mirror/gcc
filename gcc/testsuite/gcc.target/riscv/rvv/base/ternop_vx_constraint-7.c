/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */
#include "riscv_vector.h"

/*
** f0:
**  ...
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**  ...
**	ret
*/
void f0 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmacc_vx_i64m1_tumu (mask, v2, -16, v2, 4);
  vint64m1_t v4 = __riscv_vmacc_vx_i64m1_tumu (mask, v3, -16, v3, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f1:
**  ...
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**  ...
**	ret
*/
void f1 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmacc_vx_i64m1_tumu (mask,v2, 15, v2, 4);
  vint64m1_t v4 = __riscv_vmacc_vx_i64m1_tumu (mask,v3, 15, v3, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f2:
**  ...
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**  ...
**	ret
*/
void f2 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmacc_vx_i64m1_tumu (mask,v2, 16, v2, 4);
  vint64m1_t v4 = __riscv_vmacc_vx_i64m1_tumu (mask,v3, 16, v3, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f3:
**  ...
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**  ...
**	ret
*/
void f3 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmacc_vx_i64m1_tumu (mask,v2, 0xAAAAAA, v2, 4);
  vint64m1_t v4 = __riscv_vmacc_vx_i64m1_tumu (mask,v3, 0xAAAAAA, v3, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f4:
**  ...
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**  ...
**	ret
*/
void f4 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmacc_vx_i64m1_tumu (mask,v2, 0xAAAAAAA, v2, 4);
  vint64m1_t v4 = __riscv_vmacc_vx_i64m1_tumu (mask,v3, 0xAAAAAAA, v3, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f5:
**  ...
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,v0.t
**  ...
*/
void f5 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmacc_vx_i64m1_tumu (mask,v2, 0xAAAAAAAAAAAAAAAA, v2, 4);
  vint64m1_t v4 = __riscv_vmacc_vx_i64m1_tumu (mask,v3, 0xAAAAAAAAAAAAAAAA, v3, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f6:
**  ...
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,v0.t
**  ...
*/
void f6 (void * in, void *out, int64_t x, int n)
{
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmacc_vx_i64m1_tumu (mask,v2, x, v2, 4);
  vint64m1_t v4 = __riscv_vmacc_vx_i64m1_tumu (mask,v3, x, v3, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/* { dg-final { scan-assembler-not {vmv} } } */
