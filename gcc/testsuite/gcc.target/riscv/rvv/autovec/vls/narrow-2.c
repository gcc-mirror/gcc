/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_NARROW_TRUNC_XREG (int32_t, int64_t, 4)
DEF_NARROW_TRUNC_XREG (int32_t, int64_t, 8)
DEF_NARROW_TRUNC_XREG (int32_t, int64_t, 16)
DEF_NARROW_TRUNC_XREG (int32_t, int64_t, 32)
DEF_NARROW_TRUNC_XREG (int32_t, int64_t, 64)
DEF_NARROW_TRUNC_XREG (int32_t, int64_t, 128)
DEF_NARROW_TRUNC_XREG (int32_t, int64_t, 256)
DEF_NARROW_TRUNC_XREG (int32_t, int64_t, 512)

/* { dg-final { scan-assembler-times {vnsra\.wx} 8 } } */
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
