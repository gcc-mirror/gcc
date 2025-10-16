/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvl32b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 32

#include "../common/test_mixed_struct.h"
// Function under test:
//	struct_mixed_t test_mixed_struct(struct_mixed_t s)
// Check argument 1 register assignment
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+.*\).*\(reg.*:DI \d+ a1\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ \[ \.result_ptr \]\).*\(reg.*:DI \d+ a0 \[ \.result_ptr \]\)\)} "expand" } } */
// Check return value passed via pointer (result_ptr)
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ a0\).*\(reg.*:DI \d+ \[ \.result_ptr \]\)\)} "expand" } } */
