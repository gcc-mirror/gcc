/* Test for deprecation of compound expressions as lvalues.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int x, y;

void
foo (void)
{
  (x, y) = 1; /* { dg-bogus "warning" "warning in place of error" } */
}
/* { dg-error "lvalue" "compound expression as lvalue" { target *-*-* } 11 } */
