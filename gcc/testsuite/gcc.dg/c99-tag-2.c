/* Test for handling of tags.  A struct defined in an inner scope does
   not match one declared in an outer scope.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct s;
struct t { struct s *p; } x;

void
f (void)
{
  /* This is a different struct s from the outer one.  */
  struct s { int a; } y;
  x.p = &y; /* { dg-error "incompatible" } */
}
