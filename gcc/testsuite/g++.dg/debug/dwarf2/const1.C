/* { dg-do compile } */
/* { dg-options "-O -gdwarf-2 -dA -gno-strict-dwarf -fno-merge-debug-strings" } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-assembler "DW_AT_location\[^\\r\\n\]*\[\\r\\n\]*\[^\\r\\n\]*DW_OP_addr\[^\\r\\n\]*\[\\r\\n\]*\[^\\r\\n\]*fnx\[^\\r\\n\]*\[\\r\\n\]*\[^\\r\\n\]*DW_OP_stack_value" } } */

extern void fnx () __attribute__((visibility ("hidden")));
void (* const f) () = fnx;
