/* Stress test for several btf_type_tag on the same type and specified
   in various ways.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -gdwarf -dA" } */

typedef int __attribute__((btf_type_tag ("A"), btf_type_tag ("B"))) myint;

myint x;

struct S
{
  myint * __attribute__((btf_type_tag ("A"))) p;
  unsigned long __attribute__((btf_type_tag ("A")))
                __attribute__((btf_type_tag ("B")))
                __attribute__((btf_type_tag ("C"))) l;
};

char * [[gnu::btf_type_tag ("B"), gnu::btf_type_tag ("C")]] str;

unsigned long [[gnu::btf_type_tag ("B")]]
do_thing (struct S * __attribute__((btf_type_tag ("C"),
				    btf_type_tag ("B"),
				    btf_type_tag ("A"))) arg)
{
  return arg->l * 2;
}

/* Expect the following chains of annotations off of the types:
   1. int           |-> b -> a -> *
   2. myint*        |-> a -> *
   3. unsigned long |-> c -> b -> a -> *
   4. char*         |-> c -> b -> *
   4. unsigned long |-> b -> *
   5. struct S*     |-> a -> b -> c -> *

   a and b are reused in 1-3
   new c,b created in 4, reused in 5
   new a,b,c created in 5 (not yet deduplicated).  */

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_GNU_annotation" 8 } } */
/* { dg-final { scan-assembler-times " DW_AT_GNU_annotation" 11 } } */
