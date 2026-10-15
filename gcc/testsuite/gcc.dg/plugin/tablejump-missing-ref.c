/* PR middle-end/126236 */
/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fchecking -fduplicate-switch-dispatch -fno-report-bug" } */
/* { dg-prune-output {\(jump_(insn|table_data)[: ][^\n]*(\n[ \t][^\n]*)*} } */
/* { dg-prune-output "during RTL pass: tablejump_test" } */
/* { dg-ice "tablejump pattern does not reference its label" } */
/* { dg-error "tablejump pattern does not reference its label" "" { target *-*-* } 0 } */

#define TEST_NAME missing_pattern_ref
#include "tablejump-test.h"
