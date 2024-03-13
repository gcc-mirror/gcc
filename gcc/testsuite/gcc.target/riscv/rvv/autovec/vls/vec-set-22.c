/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_VEC_SET_SCALAR_INDEX (vec_set, v1df, double)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v2df, double)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v4df, double)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v8df, double)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v16df, double)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v32df, double)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v64df, double)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v128df, double)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v256df, double)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v512df, double)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vslideup\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[axt][0-9]+} 9 } } */
