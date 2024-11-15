/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-rtl-expand-details" } */

#include "sat_arith.h"

DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2 (0, uint8_t, -43)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2 (1, uint8_t, 269)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2 (2, uint8_t, 369u)

DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2 (3, uint16_t, -4)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2 (4, uint16_t, 65579)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2 (5, uint16_t, 65679u)

DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2 (6, uint32_t, -62)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2 (7, uint32_t, 4294967342ll)
DEF_SAT_U_SUB_IMM_TYPE_CHECK_FMT_2 (8, uint32_t, 4394967342ull)

/* { dg-final { scan-rtl-dump-not ".SAT_ADD " "expand" } } */
