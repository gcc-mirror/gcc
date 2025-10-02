/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32 -fdump-tree-optimized" } */

#include "sat_arith.h"

#define NT uint8_t
#define WT uint64_t

DEF_SAT_U_MUL_FMT_5_WRAP(NT, WT)

/* { dg-final { scan-tree-dump-times ".SAT_MUL" 1 "optimized" } } */
