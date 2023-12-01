/* PR middle-end/112770 */
/* { dg-do compile { target bitint128 } } */
/* { dg-options "-std=c23 -fnon-call-exceptions" } */

void
foo (void)
{
  _BitInt(128) a = 0;
  a /= 0;		/* { dg-warning "division by zero" } */
  &a;
}
