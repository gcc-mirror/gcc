/* Test for deprecation of casts as lvalues.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int x;

void
foo (void)
{
  (char) x = 1; /* { dg-bogus "warning" "warning in place of error" } */
}
/* { dg-error "lvalue" "cast as lvalue" { target *-*-* } 11 } */
