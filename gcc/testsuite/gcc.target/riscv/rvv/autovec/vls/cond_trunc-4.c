/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_CONVERT (trunc, v4sf, v4hf, 4)
DEF_COND_CONVERT (trunc, v16sf, v16hf, 16)
DEF_COND_CONVERT (trunc, v32sf, v32hf, 32)
DEF_COND_CONVERT (trunc, v64sf, v64hf, 64)
DEF_COND_CONVERT (trunc, v128sf, v128hf, 128)
DEF_COND_CONVERT (trunc, v256sf, v256hf, 256)
DEF_COND_CONVERT (trunc, v512sf, v512hf, 512)
DEF_COND_CONVERT (trunc, v1024sf, v1024hf, 1024)

DEF_COND_CONVERT (trunc, v4df, v4sf, 4)
DEF_COND_CONVERT (trunc, v16df, v16sf, 16)
DEF_COND_CONVERT (trunc, v32df, v32sf, 32)
DEF_COND_CONVERT (trunc, v64df, v64sf, 64)
DEF_COND_CONVERT (trunc, v128df, v128sf, 128)
DEF_COND_CONVERT (trunc, v256df, v256sf, 256)
DEF_COND_CONVERT (trunc, v512df, v512sf, 512)

/* { dg-final { scan-assembler-times {vfncvt\.f\.f\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 15 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-not {vmerge} } } */
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
