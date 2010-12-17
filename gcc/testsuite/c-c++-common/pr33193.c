/* PR c/33193 */
/* { dg-do compile } */

struct a {float x, y; };

float f(struct a b)
{
  /* The error messages here are different between C and C++, so just
     make sure we get an error.  */
  float x = __real b;		/* { dg-error "" } */
  float y = __imag b;		/* { dg-error "" } */
  return x / y;
}
int f1(int *b)
{
  float x = __imag b;		/* { dg-error "wrong type argument" } */
  float y = __real b;		/* { dg-error "wrong type argument" } */
  return x - y;
}
