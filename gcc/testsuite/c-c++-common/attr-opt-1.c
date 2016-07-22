/* PR c/70255 */
/* { dg-do compile } */

double
fn1 (double h, double l) /* { dg-message "previous definition" } */
{
  return h + l;
}
double fn1 (double, double) __attribute__ ((optimize ("no-associative-math"))); /* { dg-warning "optimization attribute" } */

__attribute__ ((optimize ("no-associative-math"))) double
fn2 (double h, double l)
{
  return h + l;
}
double fn2 (double, double) __attribute__ ((optimize ("no-associative-math")));

__attribute__ ((optimize ("no-associative-math"))) double
fn3 (double h, double l) /* { dg-message "previous definition" } */
{
  return h + l;
}
double fn3 (double, double) __attribute__ ((optimize ("O2,no-associative-math"))); /* { dg-warning "optimization attribute" } */

__attribute__ ((optimize ("no-associative-math,O2"))) double
fn4 (double h, double l) /* { dg-message "previous definition" } */
{
  return h + l;
}
double fn4 (double, double) __attribute__ ((optimize ("O2,no-associative-math"))); /* { dg-warning "optimization attribute" } */

__attribute__ ((optimize ("no-reciprocal-math"), optimize ("no-associative-math"))) int
fn5 (void)
{
  return 0;
}
int __attribute__ ((optimize ("no-associative-math"), optimize ("no-reciprocal-math"))) fn5 (void);
