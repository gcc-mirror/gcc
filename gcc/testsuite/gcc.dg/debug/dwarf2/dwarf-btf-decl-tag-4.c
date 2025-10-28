/* Test DWARF generation for decl_tags on global decls appearing multiple
   times with different decl_tags.  PR122248.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

#define __tag1 __attribute__((btf_decl_tag ("tag1")))
#define __tag2 __attribute__((btf_decl_tag ("tag2")))
#define __tag3 __attribute__((btf_decl_tag ("tag3")))
#define __tag4 __attribute__((btf_decl_tag ("tag4")))

int foo __tag1;
int foo __tag2;

/* Result: foo has __tag1 and __tag2.  */

int bar __tag3;
int bar;

/* Result: bar has __tag3.  */

int baz;
int baz __tag4;

/* Result: baz has __tag4.  */

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 4 } } */
/* { dg-final { scan-assembler-times " DW_AT_GNU_annotation" 4 } } */

