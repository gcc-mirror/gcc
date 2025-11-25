/* Test generating annotation DIEs for struct/union/enum types.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

enum E
{
  ONE,
  TWO
} __attribute__((btf_type_tag("foo")));

enum E some_e;

struct S
{
  int i;
  char c;
} __attribute__((btf_type_tag("foo")));

typedef struct S S1;
S1 plain_s1;
const S1 const_s1;

union U
{
  int x;
  char y;
} __attribute__((btf_type_tag("foo")));

volatile union U volatile_u;

/* One annotation DIE may be shared by all three annotated types.  */
/* { dg-final { scan-assembler-times {(?n)DIE \(.*\) DW_TAG_GNU_annotation} 1 } } */
/* { dg-final { scan-assembler-times {(?n)( DW_AT_name: "btf_type_tag"|"btf_type_tag..".* DW_AT_name)} 1 } } */
/* { dg-final { scan-assembler-times { DW_AT_GNU_annotation} 3 } } */
