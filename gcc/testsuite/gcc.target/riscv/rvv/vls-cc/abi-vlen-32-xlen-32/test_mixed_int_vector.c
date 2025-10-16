/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve32f_zvl32b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 32

#include "../common/test_mixed_int_vector.h"
// Function under test:
//	int test_mixed_int_vector(int a, int32x4_t vec, int b, int32x4_t vec2)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ vec \]\)[[:space:]]+\(reg.*:V4SI \d+ v8 \[ vec \]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ vec2 \]\)[[:space:]]+\(reg.*:V4SI \d+ v12 \[ vec2 \]\)\)} "expand" } } */
