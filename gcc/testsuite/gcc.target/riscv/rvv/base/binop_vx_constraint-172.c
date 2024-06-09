/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** f1:
**  ...
**	vslide1down\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1down\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1down\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1down\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1down\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1down\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vmerge\.vvm\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**  ...
**	ret
*/
void f1 (void * in, void *out, int64_t x, int n)
{
  vbool64_t m = __riscv_vlm_v_b64 (in, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1down_vx_i64m1 (v2, x, 2);
  vint64m1_t v4 = __riscv_vslide1down_vx_i64m1_tu (v3, v3, x, 2);
  vint64m1_t v5 = __riscv_vslide1down_vx_i64m1_tumu (m, v4, v4, x, 2);
  __riscv_vse64_v_i64m1 (out + 2, v5, 4);
}

/*
** f2:
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vslide1up\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  ...
**	vmerge\.vvm\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**  ...
**	ret
*/
void f2 (void * in, void *out, int64_t x, int n)
{
  vbool64_t m = __riscv_vlm_v_b64 (in, 4);
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vint64m1_t v3 = __riscv_vslide1up_vx_i64m1 (v2, x, 2);
  vint64m1_t v4 = __riscv_vslide1up_vx_i64m1_tu (v3, v3, x, 2);
  vint64m1_t v5 = __riscv_vslide1up_vx_i64m1_tumu (m, v4, v4, x, 2);
  __riscv_vse64_v_i64m1 (out + 2, v5, 4);
}

/* { dg-final { scan-assembler-times {vmv} 3 } } */
