/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d_zvl64b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 64

#include "../common/test_two_registers.h"
// Function under test:
//	int32x8_t test_two_registers(int32x8_t vec, int32x8_t vec2)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ \[ vec \]\)[[:space:]]+\(reg.*:V8SI \d+ v8 \[ vec \]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ \[ vec2 \]\)[[:space:]]+\(reg.*:V8SI \d+ v12 \[ vec2 \]\)\)} "expand" } } */
// Check return value passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ v8\)[[:space:]]+\(reg.*:V8SI \d+ \[ <retval>.*\]\)\)} "expand" } } */
