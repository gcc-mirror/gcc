/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve32f_zvl128b -mabi=ilp32d -mrvv-vector-bits=zvl -fno-vect-cost-model -fdump-tree-vect-details" } */

#include "template-1.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 5 "vect" } } */
