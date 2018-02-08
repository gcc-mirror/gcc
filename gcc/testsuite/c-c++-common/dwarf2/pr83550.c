/* PR debug/83550 */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA -fno-merge-debug-strings" } */

struct my_struct;
extern struct my_struct s;
struct my_struct { int a, b; };
struct my_struct q;

/* { dg-final { scan-assembler "DW_TAG_structure_type\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"my_struct\[^\\r\\n\]*DW_AT_name(\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*DW_AT_)*\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\[^0-9a-fA-FxX](0x)?7\[^0-9a-fA-FxX]\[^\\r\\n\]*DW_AT_decl_line" } } */
