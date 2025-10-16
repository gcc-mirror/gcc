/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve32f_zvl32b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 32

#include "../common/test_mixed_float_vector.h"
// Function under test:
//	float test_mixed_float_vector(float f1, float32x4_t vec, double d1, float32x4_t vec2)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SF \d+ \[ vec \]\)[[:space:]]+\(reg.*:V4SF \d+ v8 \[ vec \]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SF \d+ \[ vec2 \]\)[[:space:]]+\(reg.*:V4SF \d+ v12 \[ vec2 \]\)\)} "expand" } } */
