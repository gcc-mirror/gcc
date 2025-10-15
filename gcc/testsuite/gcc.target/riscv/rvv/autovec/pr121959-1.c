/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvbb -mabi=lp64d -O3" } */

#include "pr121959.h"

DEF_VWSLL_FUNC_0(int32_t, uint8_t, 16)

/* { dg-final { scan-assembler-times {vwsll.vi} 1 } } */
/* { dg-final { scan-assembler-not {vsll.vi} } } */
