/* Test generation for btf_type_tag attribute for pointer typedef with
   tags appearing on both the typedef and the usage of the tyepdef.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

#define __tag1 __attribute__((btf_type_tag ("tag1")))
#define __tag2 __attribute__((btf_type_tag ("tag2")))

typedef int __tag1 foo;

foo a;
foo __tag2 b;

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_name: \"btf_type_tag\"" 2 } } */

/* Due to an ambiguity in the tree attribute list, it is not currently possible
   to distinguish with certaianty whether "tag1" appears to the left or right
   of "foo" in the declaration of "b" above.  This means that the DIE for
   "tag1" is also referred in the AT_GNU_annotation chain of the duplicate
   typedef DIE annotated wtih "tag2", for a total of 3 AT_GNU_annotations.  */
/* { dg-final { scan-assembler-times " DW_AT_GNU_annotation" 3 } } */

/* A duplicate typedef die must be created for the tagged use in 'b'.  */
/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_typedef" 2 } } */
