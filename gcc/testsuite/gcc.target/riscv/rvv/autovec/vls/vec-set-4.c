/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_VEC_SET_IMM_INDEX (vec_set, v1di, int64_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v2di, int64_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v4di, int64_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v8di, int64_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v16di, int64_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v32di, int64_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v64di, int64_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v128di, int64_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v256di, int64_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v512di, int64_t, 0)

DEF_VEC_SET_IMM_INDEX (vec_set, v2di, int64_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v4di, int64_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v8di, int64_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v16di, int64_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v32di, int64_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v64di, int64_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v128di, int64_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v256di, int64_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v512di, int64_t, 1)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vmv\.s\.x\s+v[0-9]+,\s*[atx][0-9]+} 9 } } */
/* { dg-final { scan-assembler-times {vslideup\.vi\s+v[0-9]+,\s*v[0-9]+,\s*1} 9 } } */
