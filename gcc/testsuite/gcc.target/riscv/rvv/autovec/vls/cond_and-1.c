/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_BINOP (cond_and, 4, v4qi, &)
DEF_COND_BINOP (cond_and, 8, v8qi, &)
DEF_COND_BINOP (cond_and, 16, v16qi, &)
DEF_COND_BINOP (cond_and, 32, v32qi, &)
DEF_COND_BINOP (cond_and, 64, v64qi, &)
DEF_COND_BINOP (cond_and, 128, v128qi, &)
DEF_COND_BINOP (cond_and, 256, v256qi, &)
DEF_COND_BINOP (cond_and, 512, v512qi, &)
DEF_COND_BINOP (cond_and, 1024, v1024qi, &)
DEF_COND_BINOP (cond_and, 2048, v2048qi, &)
DEF_COND_BINOP (cond_and, 4096, v4096qi, &)

DEF_COND_BINOP (cond_and, 4, v4hi, &)
DEF_COND_BINOP (cond_and, 8, v8hi, &)
DEF_COND_BINOP (cond_and, 16, v16hi, &)
DEF_COND_BINOP (cond_and, 32, v32hi, &)
DEF_COND_BINOP (cond_and, 64, v64hi, &)
DEF_COND_BINOP (cond_and, 128, v128hi, &)
DEF_COND_BINOP (cond_and, 256, v256hi, &)
DEF_COND_BINOP (cond_and, 512, v512hi, &)
DEF_COND_BINOP (cond_and, 1024, v1024hi, &)
DEF_COND_BINOP (cond_and, 2048, v2048hi, &)

DEF_COND_BINOP (cond_and, 4, v4si, &)
DEF_COND_BINOP (cond_and, 8, v8si, &)
DEF_COND_BINOP (cond_and, 16, v16si, &)
DEF_COND_BINOP (cond_and, 32, v32si, &)
DEF_COND_BINOP (cond_and, 64, v64si, &)
DEF_COND_BINOP (cond_and, 128, v128si, &)
DEF_COND_BINOP (cond_and, 256, v256si, &)
DEF_COND_BINOP (cond_and, 512, v512si, &)
DEF_COND_BINOP (cond_and, 1024, v1024si, &)

DEF_COND_BINOP (cond_and, 4, v4di, &)
DEF_COND_BINOP (cond_and, 8, v8di, &)
DEF_COND_BINOP (cond_and, 16, v16di, &)
DEF_COND_BINOP (cond_and, 32, v32di, &)
DEF_COND_BINOP (cond_and, 64, v64di, &)
DEF_COND_BINOP (cond_and, 128, v128di, &)
DEF_COND_BINOP (cond_and, 256, v256di, &)
DEF_COND_BINOP (cond_and, 512, v512di, &)

DEF_COND_BINOP (cond_and, 4, v4uqi, &)
DEF_COND_BINOP (cond_and, 8, v8uqi, &)
DEF_COND_BINOP (cond_and, 16, v16uqi, &)
DEF_COND_BINOP (cond_and, 32, v32uqi, &)
DEF_COND_BINOP (cond_and, 64, v64uqi, &)
DEF_COND_BINOP (cond_and, 128, v128uqi, &)
DEF_COND_BINOP (cond_and, 256, v256uqi, &)
DEF_COND_BINOP (cond_and, 512, v512uqi, &)
DEF_COND_BINOP (cond_and, 1024, v1024uqi, &)
DEF_COND_BINOP (cond_and, 2048, v2048uqi, &)
DEF_COND_BINOP (cond_and, 4096, v4096uqi, &)

DEF_COND_BINOP (cond_and, 4, v4uhi, &)
DEF_COND_BINOP (cond_and, 8, v8uhi, &)
DEF_COND_BINOP (cond_and, 16, v16uhi, &)
DEF_COND_BINOP (cond_and, 32, v32uhi, &)
DEF_COND_BINOP (cond_and, 64, v64uhi, &)
DEF_COND_BINOP (cond_and, 128, v128uhi, &)
DEF_COND_BINOP (cond_and, 256, v256uhi, &)
DEF_COND_BINOP (cond_and, 512, v512uhi, &)
DEF_COND_BINOP (cond_and, 1024, v1024uhi, &)
DEF_COND_BINOP (cond_and, 2048, v2048uhi, &)

DEF_COND_BINOP (cond_and, 4, v4usi, &)
DEF_COND_BINOP (cond_and, 8, v8usi, &)
DEF_COND_BINOP (cond_and, 16, v16usi, &)
DEF_COND_BINOP (cond_and, 32, v32usi, &)
DEF_COND_BINOP (cond_and, 64, v64usi, &)
DEF_COND_BINOP (cond_and, 128, v128usi, &)
DEF_COND_BINOP (cond_and, 256, v256usi, &)
DEF_COND_BINOP (cond_and, 512, v512usi, &)
DEF_COND_BINOP (cond_and, 1024, v1024usi, &)

DEF_COND_BINOP (cond_and, 4, v4udi, &)
DEF_COND_BINOP (cond_and, 8, v8udi, &)
DEF_COND_BINOP (cond_and, 16, v16udi, &)
DEF_COND_BINOP (cond_and, 32, v32udi, &)
DEF_COND_BINOP (cond_and, 64, v64udi, &)
DEF_COND_BINOP (cond_and, 128, v128udi, &)
DEF_COND_BINOP (cond_and, 256, v256udi, &)
DEF_COND_BINOP (cond_and, 512, v512udi, &)

/* { dg-final { scan-assembler-times {vand\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 76 } } */
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
