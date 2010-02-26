/* PR debug/43190 */
/* { dg-options "-gdwarf-2 -dA" } */
/* { dg-final { scan-assembler "DW_TAG_structure_type\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"S\[^\\r\\n\]*DW_AT_name" } } */
/* { dg-final { scan-assembler "DW_TAG_typedef\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*\"T\[^\\r\\n\]*DW_AT_name" } } */

typedef struct S { int i; } *T;
#define M(p) ((T) (p))

void
foo (void *p)
{
  M (p)->i++;
}
