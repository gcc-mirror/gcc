/* Test dwarf generation for btf_type_tag with cv-quals and typedefs.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

#define __tag1 __attribute__((btf_type_tag ("tag1")))
#define __tag2 __attribute__((btf_type_tag ("tag2")))

typedef const int foo;
typedef int __tag1 bar;

foo __tag2 x;
const bar y;

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_name: \"btf_type_tag\"" 2 } } */
