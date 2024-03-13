/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_BINOP (cond_shift, 4, v4si, <<)
DEF_COND_BINOP (cond_shift, 8, v8si, <<)
DEF_COND_BINOP (cond_shift, 16, v16si, <<)
DEF_COND_BINOP (cond_shift, 32, v32si, <<)
DEF_COND_BINOP (cond_shift, 64, v64si, <<)
DEF_COND_BINOP (cond_shift, 128, v128si, <<)
DEF_COND_BINOP (cond_shift, 256, v256si, <<)
DEF_COND_BINOP (cond_shift, 512, v512si, <<)
DEF_COND_BINOP (cond_shift, 1024, v1024si, <<)

DEF_COND_BINOP (cond_shift, 4, v4di, <<)
DEF_COND_BINOP (cond_shift, 8, v8di, <<)
DEF_COND_BINOP (cond_shift, 16, v16di, <<)
DEF_COND_BINOP (cond_shift, 32, v32di, <<)
DEF_COND_BINOP (cond_shift, 64, v64di, <<)
DEF_COND_BINOP (cond_shift, 128, v128di, <<)
DEF_COND_BINOP (cond_shift, 256, v256di, <<)

DEF_COND_BINOP (cond_shift, 4, v4usi, <<)
DEF_COND_BINOP (cond_shift, 8, v8usi, <<)
DEF_COND_BINOP (cond_shift, 16, v16usi, <<)
DEF_COND_BINOP (cond_shift, 32, v32usi, <<)
DEF_COND_BINOP (cond_shift, 64, v64usi, <<)
DEF_COND_BINOP (cond_shift, 128, v128usi, <<)
DEF_COND_BINOP (cond_shift, 256, v256usi, <<)
DEF_COND_BINOP (cond_shift, 512, v512usi, <<)
DEF_COND_BINOP (cond_shift, 1024, v1024usi, <<)

DEF_COND_BINOP (cond_shift, 4, v4udi, <<)
DEF_COND_BINOP (cond_shift, 8, v8udi, <<)
DEF_COND_BINOP (cond_shift, 16, v16udi, <<)
DEF_COND_BINOP (cond_shift, 32, v32udi, <<)
DEF_COND_BINOP (cond_shift, 64, v64udi, <<)
DEF_COND_BINOP (cond_shift, 128, v128udi, <<)
DEF_COND_BINOP (cond_shift, 256, v256udi, <<)

/* { dg-final { scan-assembler-times {vsll\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 32 } } */
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
