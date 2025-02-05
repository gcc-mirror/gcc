/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (neg, 4, int8_t, __builtin_abs)
DEF_OP_V (neg, 8, int8_t, __builtin_abs)
DEF_OP_V (neg, 16, int8_t, __builtin_abs)
DEF_OP_V (neg, 32, int8_t, __builtin_abs)
DEF_OP_V (neg, 64, int8_t, __builtin_abs)
DEF_OP_V (neg, 128, int8_t, __builtin_abs)
DEF_OP_V (neg, 256, int8_t, __builtin_abs)
DEF_OP_V (neg, 512, int8_t, __builtin_abs)
DEF_OP_V (neg, 1024, int8_t, __builtin_abs)
DEF_OP_V (neg, 2048, int8_t, __builtin_abs)
DEF_OP_V (neg, 4096, int8_t, __builtin_abs)

DEF_OP_V (neg, 4, int16_t, __builtin_abs)
DEF_OP_V (neg, 8, int16_t, __builtin_abs)
DEF_OP_V (neg, 16, int16_t, __builtin_abs)
DEF_OP_V (neg, 32, int16_t, __builtin_abs)
DEF_OP_V (neg, 64, int16_t, __builtin_abs)
DEF_OP_V (neg, 128, int16_t, __builtin_abs)
DEF_OP_V (neg, 256, int16_t, __builtin_abs)
DEF_OP_V (neg, 512, int16_t, __builtin_abs)
DEF_OP_V (neg, 1024, int16_t, __builtin_abs)
DEF_OP_V (neg, 2048, int16_t, __builtin_abs)

DEF_OP_V (neg, 4, int32_t, __builtin_abs)
DEF_OP_V (neg, 8, int32_t, __builtin_abs)
DEF_OP_V (neg, 16, int32_t, __builtin_abs)
DEF_OP_V (neg, 32, int32_t, __builtin_abs)
DEF_OP_V (neg, 64, int32_t, __builtin_abs)
DEF_OP_V (neg, 128, int32_t, __builtin_abs)
DEF_OP_V (neg, 256, int32_t, __builtin_abs)
DEF_OP_V (neg, 512, int32_t, __builtin_abs)
DEF_OP_V (neg, 1024, int32_t, __builtin_abs)

DEF_OP_V (neg, 4, int64_t, __builtin_abs)
DEF_OP_V (neg, 8, int64_t, __builtin_abs)
DEF_OP_V (neg, 16, int64_t, __builtin_abs)
DEF_OP_V (neg, 32, int64_t, __builtin_abs)
DEF_OP_V (neg, 64, int64_t, __builtin_abs)
DEF_OP_V (neg, 128, int64_t, __builtin_abs)
DEF_OP_V (neg, 256, int64_t, __builtin_abs)
DEF_OP_V (neg, 512, int64_t, __builtin_abs)

/* { dg-final { scan-assembler-times {vrsub\.vi} 38 } } */
/* { dg-final { scan-assembler-times {vmax\.vv} 38 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-tree-dump-not "1,1" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2,2" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4,4" "optimized" } } */
/* { dg-final { scan-tree-dump-not "16,16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "32,32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "64,64" "optimized" } } */
/* { dg-final { scan-tree-dump-not "128,128" "optimized" } } */
/* { dg-final { scan-tree-dump-not "256,256" "optimized" } } */
/* { dg-final { scan-tree-dump-not "512,512" "optimized" } } */
/* { dg-final { scan-tree-dump-not "1024,1024" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2048,2048" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4096,4096" "optimized" } } */
