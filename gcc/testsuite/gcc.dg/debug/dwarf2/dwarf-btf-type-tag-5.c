/* Test generation for btf_type_tag attribute on array type.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

int arr[8] __attribute__((btf_type_tag("tagged_arr")));

/* { dg-final { scan-assembler-times {(?n)DIE \(.*\) DW_TAG_GNU_annotation} 1 } } */
/* { dg-final { scan-assembler-times {(?n)( DW_AT_name: "btf_type_tag"|"btf_type_tag..".*)} 1 } } */
/* { dg-final { scan-assembler-times {(?n)( DW_AT_const_value: "tagged_arr"|"tagged_arr..".*DW_AT_const_value)} 1 } } */
/* { dg-final { scan-assembler-times { DW_AT_GNU_annotation} 1 } } */
