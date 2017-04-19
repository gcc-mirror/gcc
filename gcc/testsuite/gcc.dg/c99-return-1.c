/* Test for constraints on return statements.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

int
foo (void)
{
  return; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "with no value" "return constraint violation" { target *-*-* } .-1 } */
}

void
bar (void)
{
  return 1; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "with a value" "return constraint violation" { target *-*-* } .-1 } */
}
