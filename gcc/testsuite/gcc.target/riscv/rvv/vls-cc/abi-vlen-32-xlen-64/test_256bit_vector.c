/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvl32b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 32

#include "../common/test_256bit_vector.h"
// Function under test:
//	int32x8_t test_256bit_vector(int32x8_t vec1, int32x8_t vec2)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ \[ vec1 \]\)[[:space:]]+\(reg.*:V8SI \d+ v8 \[ vec1 \]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+ \[ vec2 \]\)[[:space:]]+\(reg.*:V8SI \d+ v16 \[ vec2 \]\)\)} "expand" } } */
