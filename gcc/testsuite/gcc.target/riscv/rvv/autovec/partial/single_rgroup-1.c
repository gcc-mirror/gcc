/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fno-tree-loop-distribute-patterns -fdump-tree-vect-details" } */

#include "single_rgroup-1.h"

TEST_ALL (test_1)

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 10 "vect" } } */
