/* Test diagnostic for empty initializer braces.  Test with
   -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic" } */

struct s { int a; } x = { }; /* { dg-warning "ISO C forbids empty initializer braces" } */

struct s *p = &(struct s){ }; /* { dg-warning "ISO C forbids empty initializer braces" } */
