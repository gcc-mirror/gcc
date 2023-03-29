/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */
#include "riscv_vector.h"

/*
** f0:
**  ...
**	vmsge\.vi\tv[0-9]+,\s*v[0-9]+,\s*-15
**	vmsge\.vi\tv[0-9]+,\s*v[0-9]+,\s*-15,v0.t
**  ...
**	ret
*/
void f0 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmsge_vx_i64m1_b64 (v2, -15, 4);
  vbool64_t v4 = __riscv_vmsge_vx_i64m1_b64_m (v3, v2, -15, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

/*
** f1:
**  ...
**	vmsge\.vi\tv[0-9]+,\s*v[0-9]+,\s*16
**	vmsge\.vi\tv[0-9]+,\s*v[0-9]+,\s*16,v0.t
**  ...
**	ret
*/
void f1 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmsge_vx_i64m1_b64 (v2, 16, 4);
  vbool64_t v4 = __riscv_vmsge_vx_i64m1_b64_m (v3, v2, 16, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

/*
** f2:
**  ...
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  vmnot\.m\s+v[0-9]+,\s*v[0-9]+
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,v0.t
**	vmxor\.mm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**  ...
**	ret
*/
void f2 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmsge_vx_i64m1_b64 (v2, 17, 4);
  vbool64_t v4 = __riscv_vmsge_vx_i64m1_b64_m (v3, v2, 17, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

/*
** f3:
**  ...
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  vmnot\.m\s+v[0-9]+,\s*v[0-9]+
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,v0.t
**	vmxor\.mm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**  ...
**	ret
*/
void f3 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmsge_vx_i64m1_b64 (v2, 0xAAAAAAAA, 4);
  vbool64_t v4 = __riscv_vmsge_vx_i64m1_b64_m (v3, v2, 0xAAAAAAAA, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

/*
** f4:
**  ...
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  vmnot\.m\s+v[0-9]+,\s*v[0-9]+
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,v0.t
**	vmxor\.mm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**  ...
**	ret
*/
void f4 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmsge_vx_i64m1_b64 (v2, 0xAAAAAAAAAAAAAAAA, 4);
  vbool64_t v4 = __riscv_vmsge_vx_i64m1_b64_m (v3, v2, 0xAAAAAAAAAAAAAAAA, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

/*
** f5:
**  ...
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  vmnot\.m\s+v[0-9]+,\s*v[0-9]+
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,v0.t
**	vmxor\.mm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**  ...
**	ret
*/
void f5 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmsge_vx_i64m1_b64 (v2, 0xAAAAAAAAAAAAAAAA, 4);
  vbool64_t v4 = __riscv_vmsge_vx_i64m1_b64_m (v3, v2, 0xAAAAAAAAAAAAAAAA, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}

/*
** f6:
**  ...
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**  vmnot\.m\s+v[0-9]+,\s*v[0-9]+
**	vmslt\.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,v0.t
**	vmxor\.mm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**  ...
**	ret
*/
void f6 (void * in, void *out, int64_t x, int n)
{
  vint64m1_t v = __riscv_vle64_v_i64m1 (in + 1, 4);
  vint64m1_t v2 = __riscv_vle64_v_i64m1_tu (v, in + 2, 4);
  vbool64_t v3 = __riscv_vmsge_vx_i64m1_b64 (v2, x, 4);
  vbool64_t v4 = __riscv_vmsge_vx_i64m1_b64_m (v3, v2, x, 4);
  __riscv_vsm_v_b64 (out + 2, v4, 4);
}
