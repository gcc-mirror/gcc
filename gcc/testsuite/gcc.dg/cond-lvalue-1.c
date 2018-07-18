/* Test for deprecation of conditional expressions as lvalues.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int x, y, z;

void
foo (void)
{
  (x ? y : z) = 1; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "lvalue" "conditional expression as lvalue" { target *-*-* } .-1 } */
}
