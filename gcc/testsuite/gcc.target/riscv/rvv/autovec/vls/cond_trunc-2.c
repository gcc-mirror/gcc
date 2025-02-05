/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_CONVERT (trunc, v4si, v4qi, 4)
DEF_COND_CONVERT (trunc, v16si, v16qi, 16)
DEF_COND_CONVERT (trunc, v32si, v32qi, 32)
DEF_COND_CONVERT (trunc, v64si, v64qi, 64)
DEF_COND_CONVERT (trunc, v128si, v128qi, 128)
DEF_COND_CONVERT (trunc, v256si, v256qi, 256)
DEF_COND_CONVERT (trunc, v512si, v512qi, 512)
DEF_COND_CONVERT (trunc, v1024si, v1024qi, 1024)

DEF_COND_CONVERT (trunc, v4di, v4hi, 4)
DEF_COND_CONVERT (trunc, v16di, v16hi, 16)
DEF_COND_CONVERT (trunc, v32di, v32hi, 32)
DEF_COND_CONVERT (trunc, v64di, v64hi, 64)
DEF_COND_CONVERT (trunc, v128di, v128hi, 128)
DEF_COND_CONVERT (trunc, v256di, v256hi, 256)
DEF_COND_CONVERT (trunc, v512di, v512hi, 512)

DEF_COND_CONVERT (trunc, v4usi, v4uqi, 4)
DEF_COND_CONVERT (trunc, v16usi, v16uqi, 16)
DEF_COND_CONVERT (trunc, v32usi, v32uqi, 32)
DEF_COND_CONVERT (trunc, v64usi, v64uqi, 64)
DEF_COND_CONVERT (trunc, v128usi, v128uqi, 128)
DEF_COND_CONVERT (trunc, v256usi, v256uqi, 256)
DEF_COND_CONVERT (trunc, v512usi, v512uqi, 512)
DEF_COND_CONVERT (trunc, v1024usi, v1024uqi, 1024)

DEF_COND_CONVERT (trunc, v4udi, v4uhi, 4)
DEF_COND_CONVERT (trunc, v16udi, v16uhi, 16)
DEF_COND_CONVERT (trunc, v32udi, v32uhi, 32)
DEF_COND_CONVERT (trunc, v64udi, v64uhi, 64)
DEF_COND_CONVERT (trunc, v128udi, v128uhi, 128)
DEF_COND_CONVERT (trunc, v256udi, v256uhi, 256)
DEF_COND_CONVERT (trunc, v512udi, v512uhi, 512)

/* { dg-final { scan-assembler-times {vnsrl\.wi\s+v[0-9]+,\s*v[0-9]+,0,\s*v0.t} 30 } } */
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
