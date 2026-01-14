/* Testing the correct usage of attribute counted_by for anonymous
   structures with -fms-extensions.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fms-extensions" } */

#define __counted_by(member) \
    __attribute__((__counted_by__(member)))

/* Do not support the inward-to-outward counted-by field reference for 
   ms-extensions since checking the validity of such reference depends
   on unknown situation at the end of the structure definition.  */
struct bar {
  char *buf __counted_by (n); /* { dg-error "attribute is not a field declaration in the same structure as" } */
};

struct foo {
  int n;
  struct bar;
};

/* However, at the same time, support the outward-to-inward counted-by
   field reference for ms-extensions.  */
struct ids
{
  int length_ad;
  int length_na;
};

typedef union
{
  int length_hb;
  float other;
} ids_2;

struct person
{
  int age;
  int weight;
  struct ids;    // Anonymous structure, no name needed
  ids_2; // Anonymous union, no name needed
  char *address __attribute__ ((counted_by (length_ad)));
  char *hobby __attribute__ ((counted_by (length_hb)));
  char name[]  __attribute__ ((counted_by (length_na)));
};
