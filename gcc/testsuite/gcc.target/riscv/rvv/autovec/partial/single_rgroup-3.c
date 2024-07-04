/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfhmin -mabi=ilp32d -mrvv-vector-bits=scalable -fdump-tree-vect-details" } */

#include "single_rgroup-3.h"

TEST_ALL (test_1)

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 11 "vect" } } */
