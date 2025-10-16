/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl128b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 128

#include "../common/test_different_vector_elements.h"
// Function under test:
//	int test_different_vector_elements(int8x16_t byte_vec, int16x8_t short_vec,
//	                                   int32x4_t int_vec, int64x2_t long_vec)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V16QI \d+ \[ byte_vec \]\)[[:space:]]+\(reg.*:V16QI \d+ v8 \[ byte_vec \]\)\)} "expand" } } */
// Check vector argument 2 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8HI \d+ \[ short_vec \]\)[[:space:]]+\(reg.*:V8HI \d+ v9 \[ short_vec \]\)\)} "expand" } } */
// Check argument 3 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V4SI \d+ \[ int_vec \]\)[[:space:]]+\(reg.*:V4SI \d+ v10 \[ int_vec \]\)\)} "expand" } } */
// Check argument 4 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V2DI \d+ \[ long_vec \]\)[[:space:]]+\(reg.*:V2DI \d+ v11 \[ long_vec \]\)\)} "expand" } } */
