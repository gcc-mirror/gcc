/* Test simple generation of DW_TAG_GNU_annotation DIE for
   btf_decl_tag attribute.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

int *foo __attribute__((btf_decl_tag ("my_foo")));

/* { dg-final { scan-assembler-times {(?n)DIE \(.*\) DW_TAG_GNU_annotation} 1 } } */
/* { dg-final { scan-assembler-times {(?n)( DW_AT_name: "btf_decl_tag"|"btf_decl_tag..".*DW_AT_name)} 1 } } */
/* { dg-final { scan-assembler-times {(?n)( DW_AT_const_value: "my_foo"|"my_foo..".*DW_AT_const_value)} 1 } } */
/* { dg-final { scan-assembler-times { DW_AT_GNU_annotation} 1 } } */
