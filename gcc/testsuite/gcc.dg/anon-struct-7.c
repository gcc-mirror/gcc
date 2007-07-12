/* Test diagnostics for structure member with no type specifier or
   declarator.  Test with -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

struct s {
  int a;
  const; /* { dg-warning "ISO C forbids member declarations with no members" } */
};
