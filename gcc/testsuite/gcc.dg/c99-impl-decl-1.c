/* Test for implicit function declaration: in C90 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
foo (void)
{
  bar (); /* { dg-bogus "warning" "warning in place of error" } */
 /* { dg-error "implicit" "C99 implicit declaration error" { target *-*-* } 9 } */
}

/* C90 subclause 7.1.7 says we can implicitly declare strcmp; C99 removes
   implict declarations.
*/
int
bar (const char *a, const char *b)
{
  return strcmp (a, b); /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "implicit" "C99 implicit declaration error" { target *-*-* } 19 } */
}
