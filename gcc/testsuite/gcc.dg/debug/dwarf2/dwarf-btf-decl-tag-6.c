/* Test DWARF generation for decl_tags on global decls appearing multiple
   times with different decl_tags.  PR122248.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

#define __tag1 __attribute__((btf_decl_tag ("tag1")))
#define __tag2 __attribute__((btf_decl_tag ("tag2")))
#define __tag3 __attribute__((btf_decl_tag ("tag3")))

__tag1
extern int
do_thing (int);

__tag2
__tag3
int
do_thing (int x)
{
  return x * x;
}

/* Result: do_thing has all 3 tags.  */
/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 3 } } */
/* { dg-final { scan-assembler-times " DW_AT_GNU_annotation" 3 } } */
