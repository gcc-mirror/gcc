/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_FP_CONVERT (fncvt, di, hf, _Float16, 4)
DEF_COND_FP_CONVERT (fncvt, di, hf, _Float16, 16)
DEF_COND_FP_CONVERT (fncvt, di, hf, _Float16, 32)
DEF_COND_FP_CONVERT (fncvt, di, hf, _Float16, 64)
DEF_COND_FP_CONVERT (fncvt, di, hf, _Float16, 128)
DEF_COND_FP_CONVERT (fncvt, di, hf, _Float16, 256)
DEF_COND_FP_CONVERT (fncvt, di, hf, _Float16, 512)

DEF_COND_FP_CONVERT (fncvt, udi, hf, _Float16, 4)
DEF_COND_FP_CONVERT (fncvt, udi, hf, _Float16, 16)
DEF_COND_FP_CONVERT (fncvt, udi, hf, _Float16, 32)
DEF_COND_FP_CONVERT (fncvt, udi, hf, _Float16, 64)
DEF_COND_FP_CONVERT (fncvt, udi, hf, _Float16, 128)
DEF_COND_FP_CONVERT (fncvt, udi, hf, _Float16, 256)
DEF_COND_FP_CONVERT (fncvt, udi, hf, _Float16, 512)

/* TODO: Currently, we can't vectorize DI -> HF.  */
