/* Test diagnostic for empty initializer braces.  Test with no special
   options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

struct s { int a; } x = { };

struct s *p = &(struct s){ };
