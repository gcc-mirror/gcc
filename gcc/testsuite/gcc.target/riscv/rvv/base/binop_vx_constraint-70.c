/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */
#include "riscv_vector.h"

/*
** f0:
**  ...
**	vmax\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**	vmax\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f0 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmax_vx_i64m1 (v2, -16, 4);
  vint64m1_t v4 = __riscv_vmax_vx_i64m1 (v3, -16, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f1:
**  ...
**	vmax\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**	vmax\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f1 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmax_vx_i64m1 (v2, 15, 4);
  vint64m1_t v4 = __riscv_vmax_vx_i64m1 (v3, 15, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f2:
**  ...
**	vmax\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**	vmax\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f2 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmax_vx_i64m1 (v2, 16, 4);
  vint64m1_t v4 = __riscv_vmax_vx_i64m1 (v3, 16, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f3:
**  ...
**	vmax\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**	vmax\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f3 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vmax_vx_i64m1 (v2, 0xAAAAAAA, 4);
  vint64m1_t v4 = __riscv_vmax_vx_i64m1 (v3, 0xAAAAAAA, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}
