/* PR c/64440 */
/* { dg-do compile } */
/* { dg-options "-Wall -O2" } */

int
foo (int x)
{
  const int y = 0;
  int r = 0;
  r += x / y; /* { dg-warning "division by zero" } */
  r += x / 0; /* { dg-warning "division by zero" } */
  r += x % y; /* { dg-warning "division by zero" } */
  r += x % 0; /* { dg-warning "division by zero" } */
  return r;
}
