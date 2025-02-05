/* Test dwarf generation for btf_decl_tag on struct and union members.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

#define __tag1 __attribute__((btf_decl_tag ("decl1")))
#define __tag2 __attribute__((btf_decl_tag ("decl2")))

union U {
  int i __tag1;
  unsigned char ub[4];
};

struct S {
  union U u;
  int b __tag2;
  char *z __tag1;
};

struct S my_s __tag1 __tag2;

/* We must have two occurrences of one of the two annotation DIEs due to
   the different attribute sets between declarations above.  */
/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 3 } } */
/* { dg-final { scan-assembler-times " DW_AT_name: \"btf_decl_tag\"" 3 } } */
/* { dg-final { scan-assembler-times " DW_AT_GNU_annotation" 5 } } */
