/* PR middle-end/27488 */
/* { dg-do compile } */

extern double fabs(double x);
extern double sinh(double x);

void foo()
{
  fabs(sinh(0..));  /* { dg-error "decimal points" } */
}
