/* PR preprocessor/41445 */
/* Test that token after multi-line function-like macro use
   gets correct locus even when preprocessing separately.  */
/* { dg-do compile } */
/* { dg-options "-save-temps -gdwarf -O0 -dA -fno-merge-debug-strings" } */

#define A(a,b)
int varh;A(1,



  2)int vari;
int varj;

/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"varh\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0x)?8\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"vari\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0xc|12)\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"varj\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0xd|13)\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
