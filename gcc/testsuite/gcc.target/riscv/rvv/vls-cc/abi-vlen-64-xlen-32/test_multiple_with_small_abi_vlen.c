/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d_zvl64b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 64

#include "../common/test_multiple_with_small_abi_vlen.h"
// Function under test:
//	int32x4_t test_multiple_with_small_abi_vlen(int32x4_t v1, int32x4_t v2,
//	                                            int32x4_t v3, int32x4_t v4)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v1 \]\)[[:space:]]+\(reg.*:V4SI \d+ v8 \[ v1 \]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v2 \]\)[[:space:]]+\(reg.*:V4SI \d+ v10 \[ v2 \]\)\)} "expand" } } */
// Check argument 3 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v3 \]\)[[:space:]]+\(reg.*:V4SI \d+ v12 \[ v3 \]\)\)} "expand" } } */
// Check argument 4 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v4 \]\)[[:space:]]+\(reg.*:V4SI \d+ v14 \[ v4 \]\)\)} "expand" } } */
