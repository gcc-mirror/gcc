/* Test that annotation DIEs are copied as needed to the
   debug_types section (DW_UT_type for dwarf 5).  */
/* { dg-do compile } */
/* { dg-options "-gdwarf-5 -dA -fdebug-types-section" } */

int __attribute__((btf_type_tag ("A"))) foo;

struct S
{
  int x;
  char * __attribute__((btf_type_tag ("B"))) c;
} __attribute__((btf_type_tag ("A"))) some_struct;

/* The struct type is placed in DW_UT_type, and the types of both members and
   both tags are copied there too.  Since the tags and base types also exist in
   the main compile unit, we have 4 annotation DIEs total.  But since they
   are only referenced by 'foo' and by the struct members, there are only
   3 AT_GNU_annotation. The DIE for tag "B" in the main compile unit is not
   referred to by anything.  */

/* { dg-final { scan-assembler-times " DW_UT_type" 1 } } */
/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 4 } } */
/* { dg-final { scan-assembler-times " DW_AT_GNU_annotation" 3 } } */
