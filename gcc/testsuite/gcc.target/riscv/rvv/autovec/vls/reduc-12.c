/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized-details" } */

#include "def.h"

DEF_REDUC_MAXMIN (_Float16, max, >, 4)
DEF_REDUC_MAXMIN (_Float16, max, >, 8)
DEF_REDUC_MAXMIN (_Float16, max, >, 16)
DEF_REDUC_MAXMIN (_Float16, max, >, 32)
DEF_REDUC_MAXMIN (_Float16, max, >, 64)
DEF_REDUC_MAXMIN (_Float16, max, >, 128)
DEF_REDUC_MAXMIN (_Float16, max, >, 256)
DEF_REDUC_MAXMIN (_Float16, max, >, 512)
DEF_REDUC_MAXMIN (_Float16, max, >, 1024)
DEF_REDUC_MAXMIN (_Float16, max, >, 2048)

DEF_REDUC_MAXMIN (_Float16, min, <, 4)
DEF_REDUC_MAXMIN (_Float16, min, <, 8)
DEF_REDUC_MAXMIN (_Float16, min, <, 16)
DEF_REDUC_MAXMIN (_Float16, min, <, 32)
DEF_REDUC_MAXMIN (_Float16, min, <, 64)
DEF_REDUC_MAXMIN (_Float16, min, <, 128)
DEF_REDUC_MAXMIN (_Float16, min, <, 256)
DEF_REDUC_MAXMIN (_Float16, min, <, 512)
DEF_REDUC_MAXMIN (_Float16, min, <, 1024)
DEF_REDUC_MAXMIN (_Float16, min, <, 2048)

/* { dg-final { scan-assembler-times {vfredmax\.vs} 10 } } */
/* { dg-final { scan-assembler-times {vfredmin\.vs} 10 } } */
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
