/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl256b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 256

#include "../common/test_struct_four_256bit_vectors.h"
// Function under test:
//	four_256bit_vectors_struct_t test_struct_four_256bit_vectors(four_256bit_vectors_struct_t s)
// Check vector argument 1 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+\)[[:space:]]+\(reg:V8SI \d+ v8 \[ s \]\)\)} "expand" } } */
// Check vector argument 2 passed in vector register
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+\)[[:space:]]+\(reg:V8SI \d+ v9 \[ s\+32 \]\)\)} "expand" } } */
// Check argument 3 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+\)[[:space:]]+\(reg:V8SI \d+ v10 \[ s\+64 \]\)\)} "expand" } } */
// Check argument 4 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:V8SI \d+\)[[:space:]]+\(reg:V8SI \d+ v11 \[ s\+96 \]\)\)} "expand" } } */
