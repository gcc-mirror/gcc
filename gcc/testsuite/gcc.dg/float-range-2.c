/* Floating constants outside the range of their type should receive a
   pedwarn, not a warning.  This includes INFINITY if the target does
   not support infinities.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile { target vax-*-* pdp11-*-* } } */
/* { dg-options "-ansi -pedantic-errors" } */

void
f (void)
{
  float a = __builtin_inff (); /* { dg-error "target format does not support infinity" } */
  double b = __builtin_inf (); /* { dg-error "target format does not support infinity" } */
  long double c = __builtin_infl (); /* { dg-error "target format does not support infinity" } */
}
