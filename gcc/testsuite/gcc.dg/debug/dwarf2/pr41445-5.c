/* PR preprocessor/41445 */
/* Test that token after multi-line function-like macro use
   gets correct locus even when preprocessing separately.  */
/* { dg-do compile } */
/* { dg-options "-save-temps -gdwarf -O0 -dA -fno-merge-debug-strings" } */

#define A(x) vari x
#define vari(x)
#define B , varj
int A(B) ;

/*  We want to check that both vari and varj have the same line
    number.  */

/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"vari\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0xa|10)\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"varj\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0xa|10)\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
