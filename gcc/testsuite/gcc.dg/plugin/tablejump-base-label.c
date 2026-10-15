/* PR middle-end/126236 */
/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fchecking -fduplicate-switch-dispatch -fno-report-bug" } */
/* { dg-prune-output {\(jump_(insn|table_data)[: ][^\n]*(\n[ \t][^\n]*)*} } */
/* { dg-prune-output "during RTL pass: tablejump_test" } */
/* { dg-ice "jump table references a label outside the RTL chain" } */
/* { dg-error "jump table references a label outside the RTL chain" "" { target *-*-* } 0 } */

#define TEST_NAME missing_base_label
#include "tablejump-test.h"
