/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d_zvl128b -mabi=ilp32d --param riscv-autovec-preference=scalable -fdump-tree-vect-details" } */

#include "template-1.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 6 "vect" } } */
