/* PR c/79969 */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA -fno-merge-debug-strings" } */

enum ENUMTAG;

enum ENUMTAG
{
  B = 1,
  C = 2
};

void
bar (void)
{
  enum ENUMTAG a = C;
}

/* { dg-final { scan-assembler "DW_TAG_enumeration_type\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"ENUMTAG\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0x)?7\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
