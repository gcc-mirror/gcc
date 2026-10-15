/* PR middle-end/126236 */
/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fchecking -fduplicate-switch-dispatch -fno-report-bug" } */
/* { dg-prune-output {\(jump_(insn|table_data)[: ][^\n]*(\n[ \t][^\n]*)*} } */
/* { dg-prune-output "during RTL pass: tablejump_test" } */
/* { dg-ice "verify_flow_info failed" } */
/* { dg-error "jump table of bb .* does not match its successors" "" { target *-*-* } 0 } */

#define TEST_NAME extra_edge
#include "tablejump-test.h"
