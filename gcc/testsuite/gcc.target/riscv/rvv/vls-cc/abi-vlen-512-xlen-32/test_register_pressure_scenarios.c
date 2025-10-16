/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl512b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 512

#include "../common/test_register_pressure_scenarios.h"
// Function under test:
//	void test_register_pressure_scenarios(int32x2_t small1, int32x2_t small2,
//	                                      int32x4_t medium1, int32x4_t medium2,
//	                                      int32x8_t large1)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V2SI \d+ \[ small1 \]\)[[:space:]]+\(reg.*:V2SI \d+ v8 \[ small1 \]\)\)} "expand" } } */
// Check vector argument 2 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V2SI \d+ \[ small2 \]\)[[:space:]]+\(reg.*:V2SI \d+ v9 \[ small2 \]\)\)} "expand" } } */
// Check argument 3 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ medium1 \]\)[[:space:]]+\(reg.*:V4SI \d+ v10 \[ medium1 \]\)\)} "expand" } } */
// Check argument 4 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ medium2 \]\)[[:space:]]+\(reg.*:V4SI \d+ v11 \[ medium2 \]\)\)} "expand" } } */
// Check argument 5 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ \[ large1 \]\)[[:space:]]+\(reg.*:V8SI \d+ v12 \[ large1 \]\)\)} "expand" } } */
