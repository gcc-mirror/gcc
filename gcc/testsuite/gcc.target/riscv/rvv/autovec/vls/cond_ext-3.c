/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_CONVERT (sext, v4qi, v4di, 4)
DEF_COND_CONVERT (sext, v16qi, v16di, 16)
DEF_COND_CONVERT (sext, v32qi, v32di, 32)
DEF_COND_CONVERT (sext, v64qi, v64di, 64)
DEF_COND_CONVERT (sext, v128qi, v128di, 128)
DEF_COND_CONVERT (sext, v256qi, v256di, 256)
DEF_COND_CONVERT (sext, v512qi, v512di, 512)

DEF_COND_CONVERT (zext, v4uqi, v4udi, 4)
DEF_COND_CONVERT (zext, v16uqi, v16udi, 16)
DEF_COND_CONVERT (zext, v32uqi, v32udi, 32)
DEF_COND_CONVERT (zext, v64uqi, v64udi, 64)
DEF_COND_CONVERT (zext, v128uqi, v128udi, 128)
DEF_COND_CONVERT (zext, v256uqi, v256udi, 256)
DEF_COND_CONVERT (zext, v512uqi, v512udi, 512)

/* { dg-final { scan-assembler-times {vsext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 7 } } */
/* { dg-final { scan-assembler-times {vzext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 7 } } */
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
