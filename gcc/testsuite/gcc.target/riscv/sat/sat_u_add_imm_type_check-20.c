/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-rtl-expand-details" } */

#include "sat_arith.h"

DEF_SAT_U_ADD_IMM_TYPE_CHECK_FMT_1(uint32_t, 4294967342ll)

/* { dg-final { scan-rtl-dump-not ".SAT_ADD " "expand" } } */
