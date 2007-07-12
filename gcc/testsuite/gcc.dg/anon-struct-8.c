/* Test diagnostics for structure member with no type specifier or
   declarator.  Test with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

struct s {
  int a;
  const; /* { dg-error "ISO C forbids member declarations with no members" } */
};
