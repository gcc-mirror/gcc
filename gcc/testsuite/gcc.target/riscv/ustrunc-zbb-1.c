/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -O2 -fdump-tree-optimized" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32d -O2 -fdump-tree-optimized" { target { rv32 } } } */

#include "ustrunc-zbb.h"

USTRUNC_PAIRS (DEF_SAT)

/* { dg-final { scan-tree-dump-times ".SAT_TRUNC " 6 "optimized" { target { rv64 } } } } */
/* { dg-final { scan-tree-dump-times ".SAT_TRUNC " 3 "optimized" { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "\\mminu\\M" 6 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "\\mminu\\M" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-not "\\msltu\\M|\\msltiu\\M|\\mneg\\M|\\mor\\M" } } */
