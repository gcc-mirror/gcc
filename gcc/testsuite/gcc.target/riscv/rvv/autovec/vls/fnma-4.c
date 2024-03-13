/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_FNMA_VV (fnma, 2, int64_t)
DEF_FNMA_VV (fnma, 4, int64_t)
DEF_FNMA_VV (fnma, 8, int64_t)
DEF_FNMA_VV (fnma, 16, int64_t)
DEF_FNMA_VV (fnma, 32, int64_t)
DEF_FNMA_VV (fnma, 64, int64_t)
DEF_FNMA_VV (fnma, 128, int64_t)
DEF_FNMA_VV (fnma, 256, int64_t)
DEF_FNMA_VV (fnma, 512, int64_t)

DEF_FNMA_VV (fnma, 2, uint64_t)
DEF_FNMA_VV (fnma, 4, uint64_t)
DEF_FNMA_VV (fnma, 8, uint64_t)
DEF_FNMA_VV (fnma, 16, uint64_t)
DEF_FNMA_VV (fnma, 32, uint64_t)
DEF_FNMA_VV (fnma, 64, uint64_t)
DEF_FNMA_VV (fnma, 128, uint64_t)
DEF_FNMA_VV (fnma, 256, uint64_t)
DEF_FNMA_VV (fnma, 512, uint64_t)

/* { dg-final { scan-assembler-times {vnms[a-u][b-c]\.vv} 18 } } */
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
