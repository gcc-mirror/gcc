/* Test btf_type_tag and btf_decl_tag on a function.
   Here the decl_tag applies to the function, and the type_tag applies
   to the return type.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

int * __attribute__((btf_type_tag ("A")))
__attribute__((btf_decl_tag ("decl")))
foo (int *x, int *y)
{
  if (x && y && *x > *y)
    return x;
  return y;
}

/* Ideally, verify that AT_GNU_annotation in the subprogram DIE refers to
   the decl_tag annotation DIE, and the AT_GNU_annotation in the return
   type refers to the type_tag...  */
/* { dg-final { scan-assembler-times " DW_AT_name: \"btf_type_tag\"" 1 } } */
/* { dg-final { scan-assembler-times " DW_AT_name: \"btf_decl_tag\"" 1 } } */
