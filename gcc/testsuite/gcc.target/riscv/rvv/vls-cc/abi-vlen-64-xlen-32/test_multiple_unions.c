/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d_zvl64b -mabi=ilp32d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 64

#include "../common/test_multiple_unions.h"
// Function under test:
//	union_vector_t test_multiple_unions(union_vector_t u1, union_vector_t u2, union_vector_t u3)
// Check argument 1 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SI \d+.*\).*\(reg.*:SI \d+ a1\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SI \d+ \[ \.result_ptr \]\).*\(reg.*:SI \d+ a0 \[ \.result_ptr \]\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SI \d+ a0\).*\(reg.*:SI \d+ \[ \.result_ptr \]\)\)} "expand" } } */
