/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized-details" } */

#include "def.h"

DEF_REDUC_MAXMIN (float, max, >, 4)
DEF_REDUC_MAXMIN (float, max, >, 8)
DEF_REDUC_MAXMIN (float, max, >, 16)
DEF_REDUC_MAXMIN (float, max, >, 32)
DEF_REDUC_MAXMIN (float, max, >, 64)
DEF_REDUC_MAXMIN (float, max, >, 128)
DEF_REDUC_MAXMIN (float, max, >, 256)
DEF_REDUC_MAXMIN (float, max, >, 512)
DEF_REDUC_MAXMIN (float, max, >, 1024)

DEF_REDUC_MAXMIN (float, min, <, 4)
DEF_REDUC_MAXMIN (float, min, <, 8)
DEF_REDUC_MAXMIN (float, min, <, 16)
DEF_REDUC_MAXMIN (float, min, <, 32)
DEF_REDUC_MAXMIN (float, min, <, 64)
DEF_REDUC_MAXMIN (float, min, <, 128)
DEF_REDUC_MAXMIN (float, min, <, 256)
DEF_REDUC_MAXMIN (float, min, <, 512)
DEF_REDUC_MAXMIN (float, min, <, 1024)

/* { dg-final { scan-assembler-times {vfredmax\.vs} 9 } } */
/* { dg-final { scan-assembler-times {vfredmin\.vs} 9 } } */
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
