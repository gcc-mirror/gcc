/* PR preprocessor/41445 */
/* { dg-do compile } */
/* { dg-options "-g -O0 -dA -fno-merge-debug-strings" } */

#include "pr41445-5.c"

/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"vari\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0x)?7\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"varj\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0xa|10)\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
