/* Test for deprecation of compound expressions as lvalues.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int x, y;

void
foo (void)
{
  (x, y) = 1; /* { dg-warning "lvalue" "compound expression as lvalue deprecated" } */
}
