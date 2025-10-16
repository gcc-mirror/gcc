/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl128b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 128

#include "../common/test_different_vectors_struct.h"
// Function under test:
//	struct_different_vectors_t test_different_vectors_struct(struct_different_vectors_t s)
// Check argument 1 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SI \d+.*\).*\(reg.*:SI \d+ a1\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SI \d+ \[ \.result_ptr \]\).*\(reg.*:SI \d+ a0 \[ \.result_ptr \]\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SI \d+ a0\).*\(reg.*:SI \d+ \[ \.result_ptr \]\)\)} "expand" } } */
