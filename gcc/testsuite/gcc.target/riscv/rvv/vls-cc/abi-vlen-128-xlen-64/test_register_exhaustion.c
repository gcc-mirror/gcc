/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 128

#include "../common/test_register_exhaustion.h"
// Function under test:
//	int32x4_t test_register_exhaustion(int32x4_t v1, int32x4_t v2, int32x4_t v3, int32x4_t v4,
//	     int32x4_t v5, int32x4_t v6, int32x4_t v7, int32x4_t v8,
//	     int32x4_t v9, int32x4_t v10, int32x4_t v11, int32x4_t v12,
//	     int32x4_t v13, int32x4_t v14, int32x4_t v15, int32x4_t v16,
//	     int32x4_t v17)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v1 \]\)[[:space:]]+\(reg.*:V4SI \d+ v8 \[ v1 \]\)\)} "expand" } } */
// Check vector argument 2 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v2 \]\)[[:space:]]+\(reg.*:V4SI \d+ v9 \[ v2 \]\)\)} "expand" } } */
// Check argument 3 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v3 \]\)[[:space:]]+\(reg.*:V4SI \d+ v10 \[ v3 \]\)\)} "expand" } } */
// Check argument 4 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v4 \]\)[[:space:]]+\(reg.*:V4SI \d+ v11 \[ v4 \]\)\)} "expand" } } */
// Check argument 5 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v5 \]\)[[:space:]]+\(reg.*:V4SI \d+ v12 \[ v5 \]\)\)} "expand" } } */
// Check argument 6 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v6 \]\)[[:space:]]+\(reg.*:V4SI \d+ v13 \[ v6 \]\)\)} "expand" } } */
// Check argument 7 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v7 \]\)[[:space:]]+\(reg.*:V4SI \d+ v14 \[ v7 \]\)\)} "expand" } } */
// Check vector argument 8 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v8 \]\)[[:space:]]+\(reg.*:V4SI \d+ v15 \[ v8 \]\)\)} "expand" } } */
// Check vector argument 9 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v9 \]\)[[:space:]]+\(reg.*:V4SI \d+ v16 \[ v9 \]\)\)} "expand" } } */
// Check argument 10 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v10 \]\)[[:space:]]+\(reg.*:V4SI \d+ v17 \[ v10 \]\)\)} "expand" } } */
// Check argument 11 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v11 \]\)[[:space:]]+\(reg.*:V4SI \d+ v18 \[ v11 \]\)\)} "expand" } } */
// Check argument 12 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v12 \]\)[[:space:]]+\(reg.*:V4SI \d+ v19 \[ v12 \]\)\)} "expand" } } */
// Check argument 13 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v13 \]\)[[:space:]]+\(reg.*:V4SI \d+ v20 \[ v13 \]\)\)} "expand" } } */
// Check argument 14 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v14 \]\)[[:space:]]+\(reg.*:V4SI \d+ v21 \[ v14 \]\)\)} "expand" } } */
// Check argument 15 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v15 \]\)[[:space:]]+\(reg.*:V4SI \d+ v22 \[ v15 \]\)\)} "expand" } } */
// Check argument 16 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ v16 \]\)[[:space:]]+\(reg.*:V4SI \d+ v23 \[ v16 \]\)\)} "expand" } } */
