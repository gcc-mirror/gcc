/* Test generation for btf_type_tag attribute when applied to struct/union
   types after definition.  Attributes applied after definition will be
   ignored, so DW_TAG_GNU_annotations shall be generated.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

struct foo
{
  int a;
  char c;
};

struct foo __attribute__((btf_type_tag ("tag1"))) x;  /* { dg-warning "ignoring attribute" } */
typedef const struct foo c_foo;
c_foo __attribute__((btf_type_tag ("tag2"))) y; /* { dg-warning "ignoring attribute" } */

union bar
{
  int s;
  unsigned int u;
};

typedef union bar __attribute__((btf_type_tag("tag3"))) tag_bar; /* { dg-warning "ignoring attribute" } */
const tag_bar z;

/* { dg-final { scan-assembler-not "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" } } */
/* { dg-final { scan-assembler-not " DW_AT_GNU_annotation" } } */
