/* Floating constants outside the range of their type should receive a
   just a warning if the target supports infinities. Otherwise, a
   pedwarn should be produced.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-ansi -pedantic-errors -Woverflow" } */

void
f (void)
{
  float a = 1e+100000000f; /* { dg-warning "floating constant exceeds range of 'float'" } */
  double b = 1e+100000000; /* { dg-warning "floating constant exceeds range of 'double'" } */
  long double c = 1e+100000000l; /* { dg-warning "floating constant exceeds range of 'long double'" } */
}
