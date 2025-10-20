/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

#define NT uint16_t
#define WT uint32_t

DEF_SAT_U_MUL_FMT_7_WRAP(NT, WT)

/* { dg-final { scan-tree-dump-times ".SAT_MUL" 1 "optimized" } } */
