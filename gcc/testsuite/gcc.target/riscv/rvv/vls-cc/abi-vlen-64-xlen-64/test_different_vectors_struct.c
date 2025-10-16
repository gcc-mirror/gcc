/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve64d_zvl64b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 64

#include "../common/test_different_vectors_struct.h"
// Function under test:
//	struct_different_vectors_t test_different_vectors_struct(struct_different_vectors_t s)
// Check argument 1 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+.*\).*\(reg.*:DI \d+ a1\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ \[ \.result_ptr \]\).*\(reg.*:DI \d+ a0 \[ \.result_ptr \]\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ a0\).*\(reg.*:DI \d+ \[ \.result_ptr \]\)\)} "expand" } } */
