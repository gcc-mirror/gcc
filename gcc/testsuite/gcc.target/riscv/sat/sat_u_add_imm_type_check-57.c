/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3 (0, uint8_t, -43)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3 (1, uint8_t, 269)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3 (2, uint8_t, 369u)

DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3 (3, uint16_t, -4)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3 (4, uint16_t, 65579)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3 (5, uint16_t, 65679u)

DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3 (6, uint32_t, -62l)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3 (7, uint32_t, 6294967342ll)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_3 (8, uint32_t, 4394967342ull)

/* { dg-final { scan-tree-dump-not ".SAT_ADD " "optimized" } } */
