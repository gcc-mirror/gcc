/* Test generation for btf_type_tag attribute on array type.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

int arr[8] __attribute__((btf_type_tag("tagged_arr")));

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 1 } } */
/* { dg-final { scan-assembler-times " DW_AT_name: \"btf_type_tag\"" 1 } } */
/* { dg-final { scan-assembler-times " DW_AT_const_value: \"tagged_arr\"" 1 } } */
/* { dg-final { scan-assembler-times " DW_AT_GNU_annotation" 1 } } */
