/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include "bit_op_cvt.h"

DEF_BIT_CVT_0(uint8_t, int8_t, uint16_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, int8_t, uint32_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, int8_t, uint64_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, uint8_t, uint16_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, uint8_t, uint32_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, uint8_t, uint64_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, uint8_t, int16_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, uint8_t, int32_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, uint8_t, int64_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, int8_t, int16_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, int8_t, int32_t, |, bit_ior)
DEF_BIT_CVT_0(uint8_t, int8_t, int64_t, |, bit_ior)

DEF_BIT_CVT_0(uint16_t, int16_t, uint32_t, |, bit_ior)
DEF_BIT_CVT_0(uint16_t, int16_t, uint64_t, |, bit_ior)
DEF_BIT_CVT_0(uint16_t, int16_t, int32_t, |, bit_ior)
DEF_BIT_CVT_0(uint16_t, int16_t, int64_t, |, bit_ior)
DEF_BIT_CVT_0(uint16_t, uint16_t, uint32_t, |, bit_ior)
DEF_BIT_CVT_0(uint16_t, uint16_t, uint64_t, |, bit_ior)
DEF_BIT_CVT_0(uint16_t, uint16_t, int32_t, |, bit_ior)
DEF_BIT_CVT_0(uint16_t, uint16_t, int64_t, |, bit_ior)

DEF_BIT_CVT_0(uint32_t, int32_t, uint64_t, |, bit_ior)
DEF_BIT_CVT_0(uint32_t, int32_t, int64_t, |, bit_ior)
DEF_BIT_CVT_0(uint32_t, uint32_t, uint64_t, |, bit_ior)
DEF_BIT_CVT_0(uint32_t, uint32_t, int64_t, |, bit_ior)

/* { dg-final { scan-tree-dump-not "_\[0-9]\+ = \\(u?int\[0-9]\+_t\\) _\[0-9]\+;\\s+return _\[0-9]\+;" "optimized" } } */
