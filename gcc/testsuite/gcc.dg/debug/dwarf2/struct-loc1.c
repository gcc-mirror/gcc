/* { dg-do compile } */
/* { dg-options "-gdwarf -O0 -dA -fno-merge-debug-strings" } */

struct foo;
struct foo *obj;
struct foo
{
  int x;
};

int
main ()
{
  return 0;
}

/* { dg-final { scan-assembler "DW_TAG_structure_type\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"foo\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0x)?6\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
