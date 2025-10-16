/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 128

#include "../common/test_struct_five_256bit_vectors.h"
// Function under test:
//	five_256bit_vectors_struct_t test_struct_five_256bit_vectors(five_256bit_vectors_struct_t s)
// Check argument 1 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+.*\).*\(reg.*:DI \d+ a1\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ \[ \.result_ptr \]\).*\(reg.*:DI \d+ a0 \[ \.result_ptr \]\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ a0\).*\(reg.*:DI \d+ \[ \.result_ptr \]\)\)} "expand" } } */
