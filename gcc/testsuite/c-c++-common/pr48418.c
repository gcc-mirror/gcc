/* PR c/48418 */
/* { dg-do compile } */
/* { dg-options "-Wall -O2" } */

int
foo (int x)
{
  const int a = sizeof (int) * __CHAR_BIT__;
  const int b = -7;
  int c = 0;
  c += x << a;				   /* { dg-warning "left shift count >= width of type" } */
  c += x << b;				   /* { dg-warning "left shift count is negative" } */
  c += x << (sizeof (int) * __CHAR_BIT__); /* { dg-warning "left shift count >= width of type" } */
  c += x << -7;				   /* { dg-warning "left shift count is negative" } */
  c += x >> a;				   /* { dg-warning "right shift count >= width of type" } */
  c += x >> b;				   /* { dg-warning "right shift count is negative" } */
  c += x >> (sizeof (int) * __CHAR_BIT__); /* { dg-warning "right shift count >= width of type" } */
  c += x >> -7;				   /* { dg-warning "right shift count is negative" } */
  return c;
}
