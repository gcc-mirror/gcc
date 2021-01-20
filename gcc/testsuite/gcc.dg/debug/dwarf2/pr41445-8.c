/* PR preprocessor/41445 DWARF5 variant*/
/* { dg-do compile } */
/* { dg-options "-gdwarf-5 -O0 -dA -fno-merge-debug-strings" } */

#include "pr41445-5.c"

/*  We want to check that both vari and varj have the same line
    number.  */

/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"vari\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^\\r\\n\]*DW_AT_decl_line \\((0xa|10)\\)" } } */
/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"varj\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^\\r\\n\]*DW_AT_decl_line \\((0xa|10)\\)" } } */
