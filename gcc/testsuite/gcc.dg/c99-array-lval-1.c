/* Test for non-lvalue arrays decaying to pointers: in C99 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct s { char c[1]; };

extern struct s foo (void);

void
bar (void)
{
  char *t;
  (foo ()).c[0]; /* { dg-bogus "non-lvalue" "array not decaying to lvalue" } */
  t = (foo ()).c; /* { dg-bogus "non-lvalue" "array not decaying to lvalue" } */
  (foo ()).c + 1; /* { dg-bogus "non-lvalue" "array not decaying to lvalue" } */
}
