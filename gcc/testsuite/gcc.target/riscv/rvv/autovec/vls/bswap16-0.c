/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (bswap16, 1, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 2, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 4, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 8, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 16, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 32, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 64, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 128, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 256, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 512, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 1024, uint16_t, __builtin_bswap16)
DEF_OP_V (bswap16, 2048, uint16_t, __builtin_bswap16)

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
/* { dg-final { scan-assembler-times {vsrl\.vi\s+v[0-9]+,\s*v[0-9]+,\s*8} 11 } } */
/* { dg-final { scan-assembler-times {vsll\.vi\s+v[0-9]+,\s*v[0-9]+,\s*8} 11 } } */
/* { dg-final { scan-assembler-times {vor\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 11 } } */
