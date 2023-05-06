/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve32f -mabi=ilp32d --param riscv-autovec-preference=fixed-vlmax -fdump-tree-vect-details" } */

#include "template-1.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 1 "vect" } } */
