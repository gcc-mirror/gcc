/* Test for C99 mixed declarations and code: not in C90.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

void
foo (void)
{
  int i;
  i = 0;
  int j; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "mix|parse" "mixed declarations and code not in C90" { target *-*-* } 11 } */
}
