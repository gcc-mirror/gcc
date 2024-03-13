/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_VEC_SET_IMM_INDEX (vec_set, v1sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v2sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v4sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v8sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v16sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v32sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v64sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v128sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v256sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v512sf, float, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v1024sf, float, 0)

DEF_VEC_SET_IMM_INDEX (vec_set, v2sf, float, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v4sf, float, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v8sf, float, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v16sf, float, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v32sf, float, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v64sf, float, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v128sf, float, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v256sf, float, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v512sf, float, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v1024sf, float, 1)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vfmv\.s\.f\s+v[0-9]+,\s*[fa]+[0-9]+} 10 } } */
/* { dg-final { scan-assembler-times {vslideup\.vi\s+v[0-9]+,\s*v[0-9]+,\s*1} 10 } } */
