/* Test diagnostic for empty initializer braces.  Test with
   -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

struct s { int a; } x = { }; /* { dg-error "error: ISO C forbids empty initializer braces" } */

struct s *p = &(struct s){ }; /* { dg-error "error: ISO C forbids empty initializer braces" } */
