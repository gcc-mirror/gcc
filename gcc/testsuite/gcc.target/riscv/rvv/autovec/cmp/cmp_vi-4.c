/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-max-lmul=dynamic -fdump-tree-optimized-details" } */

#include "macro.h"

CMP_VI (ne_char, char, 4, !=, -16)
CMP_VI (ne_short, short, 4, !=, -16)
CMP_VI (ne_int, int, 4, !=, -16)
CMP_VI (ne_long, long, 4, !=, -16)
CMP_VI (ne_unsigned_char, unsigned char, 4, !=, -16)
CMP_VI (ne_unsigned_short, unsigned short, 4, !=, -16)
CMP_VI (ne_unsigned_int, unsigned int, 4, !=, -16)
CMP_VI (ne_unsigned_long, unsigned long, 4, !=, -16)

/* { dg-final { scan-assembler-times {vmsne\.vi} 20 } } */
/* { dg-final { scan-assembler-not {vmsne\.vv} } } */
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
