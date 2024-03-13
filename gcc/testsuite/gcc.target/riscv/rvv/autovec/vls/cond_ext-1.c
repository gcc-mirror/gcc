/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_CONVERT (sext, v4qi, v4hi, 4)
DEF_COND_CONVERT (sext, v16qi, v16hi, 16)
DEF_COND_CONVERT (sext, v32qi, v32hi, 32)
DEF_COND_CONVERT (sext, v64qi, v64hi, 64)
DEF_COND_CONVERT (sext, v128qi, v128hi, 128)
DEF_COND_CONVERT (sext, v256qi, v256hi, 256)
DEF_COND_CONVERT (sext, v512qi, v512hi, 512)
DEF_COND_CONVERT (sext, v1024qi, v1024hi, 1024)

DEF_COND_CONVERT (sext, v4hi, v4si, 4)
DEF_COND_CONVERT (sext, v16hi, v16si, 16)
DEF_COND_CONVERT (sext, v32hi, v32si, 32)
DEF_COND_CONVERT (sext, v64hi, v64si, 64)
DEF_COND_CONVERT (sext, v128hi, v128si, 128)
DEF_COND_CONVERT (sext, v256hi, v256si, 256)
DEF_COND_CONVERT (sext, v512hi, v512si, 512)
DEF_COND_CONVERT (sext, v1024hi, v1024si, 1024)

DEF_COND_CONVERT (sext, v4si, v4di, 4)
DEF_COND_CONVERT (sext, v16si, v16di, 16)
DEF_COND_CONVERT (sext, v32si, v32di, 32)
DEF_COND_CONVERT (sext, v64si, v64di, 64)
DEF_COND_CONVERT (sext, v128si, v128di, 128)
DEF_COND_CONVERT (sext, v256si, v256di, 256)
DEF_COND_CONVERT (sext, v512si, v512di, 512)

DEF_COND_CONVERT (zext, v4uqi, v4uhi, 4)
DEF_COND_CONVERT (zext, v16uqi, v16uhi, 16)
DEF_COND_CONVERT (zext, v32uqi, v32uhi, 32)
DEF_COND_CONVERT (zext, v64uqi, v64uhi, 64)
DEF_COND_CONVERT (zext, v128uqi, v128uhi, 128)
DEF_COND_CONVERT (zext, v256uqi, v256uhi, 256)
DEF_COND_CONVERT (zext, v512uqi, v512uhi, 512)
DEF_COND_CONVERT (zext, v1024uqi, v1024uhi, 1024)

DEF_COND_CONVERT (zext, v4uhi, v4usi, 4)
DEF_COND_CONVERT (zext, v16uhi, v16usi, 16)
DEF_COND_CONVERT (zext, v32uhi, v32usi, 32)
DEF_COND_CONVERT (zext, v64uhi, v64usi, 64)
DEF_COND_CONVERT (zext, v128uhi, v128usi, 128)
DEF_COND_CONVERT (zext, v256uhi, v256usi, 256)
DEF_COND_CONVERT (zext, v512uhi, v512usi, 512)
DEF_COND_CONVERT (zext, v1024uhi, v1024usi, 1024)

DEF_COND_CONVERT (zext, v4usi, v4udi, 4)
DEF_COND_CONVERT (zext, v16usi, v16udi, 16)
DEF_COND_CONVERT (zext, v32usi, v32udi, 32)
DEF_COND_CONVERT (zext, v64usi, v64udi, 64)
DEF_COND_CONVERT (zext, v128usi, v128udi, 128)
DEF_COND_CONVERT (zext, v256usi, v256udi, 256)
DEF_COND_CONVERT (zext, v512usi, v512udi, 512)

/* { dg-final { scan-assembler-times {vsext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 23 } } */
/* { dg-final { scan-assembler-times {vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 23 } } */
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
