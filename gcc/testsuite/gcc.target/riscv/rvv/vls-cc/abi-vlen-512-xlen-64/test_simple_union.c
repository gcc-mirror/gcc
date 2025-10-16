/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#define ABI_VLEN 512

#include "../common/test_simple_union.h"
// Function under test:
//	union_vector_t test_simple_union(union_vector_t u)
// Check argument 1 register assignment
/* { dg-final { scan-rtl-dump {\(set \(subreg:DI \(reg.*:TI \d+ \[[^\]]+\]\) 0\).*\(reg.*:DI \d+ a0 \[[^\]]+\]\)\)} "expand" } } */
// Check argument 2 register assignment
/* { dg-final { scan-rtl-dump {\(set \(subreg:DI \(reg.*:TI \d+ \[[^\]]+\]\) [0-9]+\).*\(reg.*:DI \d+ a1 \[[^\]]+\]\)\)} "expand" } } */
// Check return value passed via integer registers using subreg
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ a0\).*\(subreg:DI \(reg.*:TI \d+ \[.*<retval>.*\]\) 0\)\)} "expand" } } */
// Check return value passed via integer registers using subreg
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ a1 \[[^\]]*\]\).*\(subreg:DI \(reg.*:TI \d+ \[.*<retval>.*\]\) \d+\)\)} "expand" } } */
