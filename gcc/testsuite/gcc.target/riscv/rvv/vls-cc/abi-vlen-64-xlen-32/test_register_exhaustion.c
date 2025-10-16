/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d_zvl64b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 64

#include "../common/test_register_exhaustion.h"
// Function under test:
//	int32x4_t test_register_exhaustion(int32x4_t v1, int32x4_t v2, int32x4_t v3, int32x4_t v4,
//	     int32x4_t v5, int32x4_t v6, int32x4_t v7, int32x4_t v8,
//	     int32x4_t v9, int32x4_t v10, int32x4_t v11, int32x4_t v12,
//	     int32x4_t v13, int32x4_t v14, int32x4_t v15, int32x4_t v16,
//	     int32x4_t v17)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v1 \]\)[[:space:]]+\(reg.*:V4SI \d+ v8 \[ v1 \]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v2 \]\)[[:space:]]+\(reg.*:V4SI \d+ v10 \[ v2 \]\)\)} "expand" } } */
// Check argument 3 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v3 \]\)[[:space:]]+\(reg.*:V4SI \d+ v12 \[ v3 \]\)\)} "expand" } } */
// Check argument 4 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v4 \]\)[[:space:]]+\(reg.*:V4SI \d+ v14 \[ v4 \]\)\)} "expand" } } */
// Check argument 5 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v5 \]\)[[:space:]]+\(reg.*:V4SI \d+ v16 \[ v5 \]\)\)} "expand" } } */
// Check argument 6 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v6 \]\)[[:space:]]+\(reg.*:V4SI \d+ v18 \[ v6 \]\)\)} "expand" } } */
// Check argument 7 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v7 \]\)[[:space:]]+\(reg.*:V4SI \d+ v20 \[ v7 \]\)\)} "expand" } } */
// Check vector argument 8 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v8 \]\)[[:space:]]+\(reg.*:V4SI \d+ v22 \[ v8 \]\)\)} "expand" } } */
