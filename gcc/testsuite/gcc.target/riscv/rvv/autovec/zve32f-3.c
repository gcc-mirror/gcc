/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve32f -mabi=ilp32d -fno-vect-cost-model -mrvv-vector-bits=zvl --param riscv-autovec-lmul=m2 -fdump-tree-vect-details" } */

#include "template-1.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 5 "vect" } } */
