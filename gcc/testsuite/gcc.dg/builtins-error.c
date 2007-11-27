/* { dg-do compile } */

struct X { int x; };

int test1(struct X x)
{
  return __builtin_isnormal(x); /* { dg-error "non-floating-point argument" } */
}

int test2(double x)
{
  return __builtin_isgreater(x); /* { dg-error "too few arguments" } */
}

int test3(double x)
{
  return __builtin_isinf(x, x); /* { dg-error "too many arguments" } */
}
