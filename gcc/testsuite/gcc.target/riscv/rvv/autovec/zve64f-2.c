/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64f -mabi=ilp32d -fno-vect-cost-model -mrvv-vector-bits=zvl -fdump-tree-vect-details" } */

#include "template-1.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 5 "vect" } } */
