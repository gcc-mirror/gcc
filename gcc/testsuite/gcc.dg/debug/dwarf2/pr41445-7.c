/* PR preprocessor/41445 DWARF5 variant */
/* Test that token after multi-line function-like macro use
   gets correct locus even when preprocessing separately.
   If lines are inserted, the expected line number must be updated.  */
/* { dg-do compile } */
/* { dg-options "-save-temps -gdwarf-5 -O0 -dA -fno-merge-debug-strings" } */

#define A(x) vari x
#define vari(x)
#define B , varj
int A(B) ;

/*  We want to check that both vari and varj have the same line
    number.  */

/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"vari\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^\\r\\n\]*DW_AT_decl_line \\((0xb|11)\\)" } } */
/* { dg-final { scan-assembler "DW_TAG_variable\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"varj\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^\\r\\n\]*DW_AT_decl_line \\((0xb|11)\\)" } } */
