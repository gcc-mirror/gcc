/* Test DWARF generation for decl_tags on global decls appearing multiple
   times with different decl_tags.  PR122248.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

#define __tag1 __attribute__((btf_decl_tag ("tag1")))
#define __tag2 __attribute__((btf_decl_tag ("tag2")))
#define __tag3 __attribute__((btf_decl_tag ("tag3")))

struct S
{
  int x;
  char c;
};

extern struct S foo __tag1;
struct S foo __tag2;

/* Result: non-completing variable DIE for 'foo' has tag1, and the
   completing DIE (with AT_specification) for 'foo' has tag2 -> tag1.  */

extern int a __tag3;
int a;

/* Result: non-completing variable DIE for a has tag3, and the
   completing DIE (with AT_specification) for 'a' also refers to tag3.  */

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 3 } } */

/* 5 AT_GNU annotations:
   - foo -> tag1
   - foo -> tag2 -> tag1
   - a -> tag3
   - a -> tag3 */
/* { dg-final { scan-assembler-times " DW_AT_GNU_annotation" 5 } } */
