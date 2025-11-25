/* Test that DW_TAG_GNU_annotation DIEs for attribute btf_type_tag are shared
   where possible.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

#define __tag1 __attribute__((btf_type_tag("tag1")))
#define __tag2 __attribute__((btf_type_tag("tag2")))

int __tag1 foo;
char * __tag1 __tag2 bar;

struct S
{
  unsigned char bytes[8];
  unsigned long __tag1 t;
  void *ptr;
};

struct S * __tag1 __tag2 my_S;

/* Only 2 DW_TAG_GNU_annotation DIEs should be generated, one each for "tag1"
   and "tag2", and they should be reused.  */
/* { dg-final { scan-assembler-times {(?n)DIE \(.*\) DW_TAG_GNU_annotation} 2 } } */
/* { dg-final { scan-assembler-times { DW_AT_name: "btf_type_tag"} 2 } } */
/* { dg-final { scan-assembler-times {(?n)( DW_AT_const_value: "tag1"|"tag1..".* DW_AT_const_value)} 1 } } */
/* { dg-final { scan-assembler-times {(?n)( DW_AT_const_value: "tag2"|"tag2..".*DW_AT_const_value)} 1 } } */

/* Each attribute-ed type shall refer via DW_AT_GNU_annotation to the
   appropriate annotation DIE, including the annotation DIE for "tag2" which
   is always chained to the DIE for "tag1" in this construction.  */
/* { dg-final { scan-assembler-times { DW_AT_GNU_annotation} 5 } } */
