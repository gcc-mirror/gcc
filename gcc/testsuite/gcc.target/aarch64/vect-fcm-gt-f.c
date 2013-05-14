/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-all -fno-unroll-loops --save-temps -fno-inline" } */

#define FTYPE float
#define ITYPE int
#define OP >
#define INV_OP <=

#include "vect-fcm.x"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 8 "vect" } } */
/* { dg-final { scan-assembler "fcmgt\\tv\[0-9\]+\.\[24\]s, v\[0-9\]+\.\[24\]s, v\[0-9\]+\.\[24\]s" } } */
/* { dg-final { scan-assembler "fcmgt\\tv\[0-9\]+\.\[24\]s, v\[0-9\]+\.\[24\]s, 0" } } */
/* { dg-final { scan-assembler "fcmle\\tv\[0-9\]+\.\[24\]s, v\[0-9\]+\.\[24\]s, 0" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
/* { dg-final { cleanup-saved-temps } } */
