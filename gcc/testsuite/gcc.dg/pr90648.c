/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O" } */

extern double copysign ();
double foo (double x)
{
  return x * copysign (); /* { dg-warning "too few arguments" } */
}
