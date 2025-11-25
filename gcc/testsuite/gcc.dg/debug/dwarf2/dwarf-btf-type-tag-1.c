/* Test simple generation for btf_type_tag attribute.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

int * __attribute__((btf_type_tag("__user"))) ptr;

/* { dg-final { scan-assembler-times {(?n)DIE \(.*\) DW_TAG_GNU_annotation} 1 } } */
/* { dg-final { scan-assembler-times {(?n)( DW_AT_name: "btf_type_tag"| "btf_type_tag..".*DW_AT_name)} 1 } } */
/* { dg-final { scan-assembler-times {(?n)( DW_AT_const_value: "__user"|"__user..".*DW_AT_const_value)} 1 } } */
/* { dg-final { scan-assembler-times { DW_AT_GNU_annotation} 1 } } */
