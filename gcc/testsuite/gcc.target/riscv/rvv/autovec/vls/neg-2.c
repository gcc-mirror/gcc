/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (neg, 2, _Float16, -)
DEF_OP_V (neg, 4, _Float16, -)
DEF_OP_V (neg, 8, _Float16, -)
DEF_OP_V (neg, 16, _Float16, -)
DEF_OP_V (neg, 32, _Float16, -)
DEF_OP_V (neg, 64, _Float16, -)
DEF_OP_V (neg, 128, _Float16, -)
DEF_OP_V (neg, 256, _Float16, -)
DEF_OP_V (neg, 512, _Float16, -)
DEF_OP_V (neg, 1024, _Float16, -)
DEF_OP_V (neg, 2048, _Float16, -)

DEF_OP_V (neg, 2, float, -)
DEF_OP_V (neg, 4, float, -)
DEF_OP_V (neg, 8, float, -)
DEF_OP_V (neg, 16, float, -)
DEF_OP_V (neg, 32, float, -)
DEF_OP_V (neg, 64, float, -)
DEF_OP_V (neg, 128, float, -)
DEF_OP_V (neg, 256, float, -)
DEF_OP_V (neg, 512, float, -)
DEF_OP_V (neg, 1024, float, -)

DEF_OP_V (neg, 2, double, -)
DEF_OP_V (neg, 4, double, -)
DEF_OP_V (neg, 8, double, -)
DEF_OP_V (neg, 16, double, -)
DEF_OP_V (neg, 32, double, -)
DEF_OP_V (neg, 64, double, -)
DEF_OP_V (neg, 128, double, -)
DEF_OP_V (neg, 256, double, -)
DEF_OP_V (neg, 512, double, -)

/* { dg-final { scan-assembler-times {vfneg\.v\s+v[0-9]+,\s*v[0-9]+} 30 } } */
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
