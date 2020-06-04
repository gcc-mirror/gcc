/* PR middle-end/71581 */
/* { dg-do compile } */
/* { dg-options "-Wuninitialized" } */

_Complex float
f1 (void)
{
  float x;
  return x;	/* { dg-warning "is used uninitialized" } */
}

_Complex double
f2 (void)
{
  double x;
  return x;	/* { dg-warning "is used uninitialized" } */
}

_Complex int
f3 (void)
{
  int x;
  return x;	/* { dg-warning "is used uninitialized" } */
}
