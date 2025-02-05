/* Test dwarf generation for btf_decl_tag on functions and function args. */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

#define __tag1 __attribute__((btf_decl_tag ("decl1")))
#define __tag2 __attribute__((btf_decl_tag ("decl2")))

int __tag1 __tag2 func (int arg_a __tag1, int arg_b __tag2)
{
  return arg_a * arg_b;
}

int foo (int x) {
  return func (x, x + 1);
}

/* In this case one of the decl tag DIEs must be duplicated due to differing
   DW_AT_GNU_annotation chain between the three uses.  */
/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 3 } } */
/* { dg-final { scan-assembler-times " DW_AT_name: \"btf_decl_tag\"" 3 } } */
/* { dg-final { scan-assembler-times " DW_AT_GNU_annotation" 4 } } */
