/* Test for error on casts as lvalues.  Casts to same type.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int x;

void
foo (void)
{
  (int) x = 1; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "lvalue" "cast as lvalue" { target *-*-* } .-1 } */
}
