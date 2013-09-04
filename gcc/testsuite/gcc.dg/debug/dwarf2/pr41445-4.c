/* PR preprocessor/41445 */
/* { dg-do compile } */
/* { dg-options "-gdwarf -O0 -dA -fno-merge-debug-strings" } */

#include "pr41445-3.c"

/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"varh\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0x)?8\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"vari\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0xc|12)\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"varj\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0xd|13)\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
