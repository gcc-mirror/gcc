/* Test for implicit function declaration: in C90 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

void
foo (void)
{
  bar ();
}

/* C90 subclause 7.1.7 says we can implicitly declare strcmp; C99 removes
   implict declarations.
*/
int
bar (const char *a, const char *b)
{
  /* This fails for GCC CVS 20000709, but is not marked XFAIL since
     GCC 2.95.2 passes.
  */
  return strcmp (a, b); /* { dg-bogus "implicit" "implicit declaration warning for strcmp" } */
}
