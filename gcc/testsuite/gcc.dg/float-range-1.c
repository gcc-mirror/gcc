/* Floating constants outside the range of their type should receive a
   pedwarn, not a warning.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-ansi -pedantic-errors" } */

void
f (void)
{
  float a = 1e+100000000f; /* { dg-error "error: floating constant exceeds range of 'float'" } */
  double b = 1e+100000000; /* { dg-error "error: floating constant exceeds range of 'double'" } */
  long double c = 1e+100000000l; /* { dg-error "error: floating constant exceeds range of 'long double'" } */
}
