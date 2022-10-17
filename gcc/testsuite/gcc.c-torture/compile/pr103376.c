/* { dg-additional-options "-Ofast" } */
__attribute__ ((optimize ("no-associative-math"))) double
fn3 (double h, double l)
{
  return h + l;
}

double fn3 (double, double) __attribute__ ((optimize ("O2,no-associative-math")));

