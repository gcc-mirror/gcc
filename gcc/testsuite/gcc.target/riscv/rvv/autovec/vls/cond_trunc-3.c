/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_CONVERT (trunc, v4di, v4qi, 4)
DEF_COND_CONVERT (trunc, v16di, v16qi, 16)
DEF_COND_CONVERT (trunc, v32di, v32qi, 32)
DEF_COND_CONVERT (trunc, v64di, v64qi, 64)
DEF_COND_CONVERT (trunc, v128di, v128qi, 128)
DEF_COND_CONVERT (trunc, v256di, v256qi, 256)
DEF_COND_CONVERT (trunc, v512di, v512qi, 512)

DEF_COND_CONVERT (trunc, v4udi, v4uqi, 4)
DEF_COND_CONVERT (trunc, v16udi, v16uqi, 16)
DEF_COND_CONVERT (trunc, v32udi, v32uqi, 32)
DEF_COND_CONVERT (trunc, v64udi, v64uqi, 64)
DEF_COND_CONVERT (trunc, v128udi, v128uqi, 128)
DEF_COND_CONVERT (trunc, v256udi, v256uqi, 256)
DEF_COND_CONVERT (trunc, v512udi, v512uqi, 512)

/* { dg-final { scan-assembler-times {vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 14 } } */
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
