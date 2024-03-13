/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fno-trapping-math -fdump-tree-optimized" } */

#include "def.h"

DEF_CONVERT (fncvt, int64_t, _Float16, 4)
DEF_CONVERT (fncvt, int64_t, _Float16, 16)
DEF_CONVERT (fncvt, int64_t, _Float16, 32)
DEF_CONVERT (fncvt, int64_t, _Float16, 64)
DEF_CONVERT (fncvt, int64_t, _Float16, 128)
DEF_CONVERT (fncvt, int64_t, _Float16, 256)
DEF_CONVERT (fncvt, int64_t, _Float16, 512)

DEF_CONVERT (fncvt, uint64_t, _Float16, 4)
DEF_CONVERT (fncvt, uint64_t, _Float16, 16)
DEF_CONVERT (fncvt, uint64_t, _Float16, 32)
DEF_CONVERT (fncvt, uint64_t, _Float16, 64)
DEF_CONVERT (fncvt, uint64_t, _Float16, 128)
DEF_CONVERT (fncvt, uint64_t, _Float16, 256)
DEF_CONVERT (fncvt, uint64_t, _Float16, 512)

/* TODO: Currently, we can't vectorize this case.  */
