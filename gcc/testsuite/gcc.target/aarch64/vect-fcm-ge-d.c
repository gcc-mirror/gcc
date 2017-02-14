/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-all -fno-unroll-loops --save-temps -fno-inline -fno-vect-cost-model" } */

#define FTYPE double
#define ITYPE long
#define OP >=
#define INV_OP <

#include "vect-fcm.x"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 8 "vect" } } */
/* { dg-final { scan-assembler "fcmge\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "fcmge\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, 0" } } */
/* { dg-final { scan-assembler "fcmlt\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, 0" } } */
