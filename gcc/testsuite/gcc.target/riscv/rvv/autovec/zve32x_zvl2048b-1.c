/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve32x_zvl2048b -mabi=ilp32d --param riscv-autovec-preference=scalable -fdump-tree-vect-details" } */

#include "template-1.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 3 "vect" } } */
