/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** f0:
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f0 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1up_vx_i64m1 (v2, -16, 4);
  vint64m1_t v4 = __riscv_vslide1up_vx_i64m1 (v3, -16, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f1:
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f1 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1up_vx_i64m1 (v2, 15, 4);
  vint64m1_t v4 = __riscv_vslide1up_vx_i64m1 (v3, 15, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f2:
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f2 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1up_vx_i64m1 (v2, 16, 4);
  vint64m1_t v4 = __riscv_vslide1up_vx_i64m1 (v3, 16, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f3:
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*zero
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*zero
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f3 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1up_vx_i64m1 (v2, 0xAAAAAAAA, 4);
  vint64m1_t v4 = __riscv_vslide1up_vx_i64m1 (v3, 0xAAAAAAAA, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f4:
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f4 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1up_vx_i64m1 (v2, 0xAAAAAAAAAAAAAAAA, 4);
  vint64m1_t v4 = __riscv_vslide1up_vx_i64m1 (v3, 0xAAAAAAAAAAAAAAAA, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f5:
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f5 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1up_vx_i64m1 (v2, 0xAAAAAAAAAAAAAAAA, 4);
  vint64m1_t v4 = __riscv_vslide1up_vx_i64m1 (v3, 0xAAAAAAAAAAAAAAAA, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f6:
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	ret
*/
void f6 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1up_vx_i64m1 (v2, x, 4);
  vint64m1_t v4 = __riscv_vslide1up_vx_i64m1 (v3, x, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/*
** f7:
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*zero
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*zero
**  ...
**	ret
*/
void f7 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1up_vx_i64m1 (v2, 0, 4);
  vint64m1_t v4 = __riscv_vslide1up_vx_i64m1 (v3, 0, 4);
  __riscv_vse64_v_i64m1 (out + 2, v4, 4);
}

/* { dg-final { scan-assembler-not {vmv} } } */
