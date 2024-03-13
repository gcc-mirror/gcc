/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_VEC_SET_IMM_INDEX (vec_set, v1df, double, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v2df, double, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v4df, double, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v8df, double, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v16df, double, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v32df, double, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v64df, double, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v128df, double, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v256df, double, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v512df, double, 0)

DEF_VEC_SET_IMM_INDEX (vec_set, v2df, double, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v4df, double, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v8df, double, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v16df, double, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v32df, double, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v64df, double, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v128df, double, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v256df, double, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v512df, double, 1)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vfmv\.s\.f\s+v[0-9]+,\s*[fa]+[0-9]+} 9 } } */
/* { dg-final { scan-assembler-times {vslideup\.vi\s+v[0-9]+,\s*v[0-9]+,\s*1} 9 } } */
