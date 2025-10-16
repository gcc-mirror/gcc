/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 256

#include "../common/test_two_registers.h"
// Function under test:
//	int32x8_t test_two_registers(int32x8_t vec, int32x8_t vec2)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ \[ vec \]\)[[:space:]]+\(reg.*:V8SI \d+ v8 \[ vec \]\)\)} "expand" } } */
// Check vector argument 2 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ \[ vec2 \]\)[[:space:]]+\(reg.*:V8SI \d+ v9 \[ vec2 \]\)\)} "expand" } } */
// Check return value passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ v8\)[[:space:]]+\(reg.*:V8SI \d+ \[ <retval>.*\]\)\)} "expand" } } */
