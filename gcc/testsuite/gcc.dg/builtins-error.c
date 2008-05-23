/* { dg-do compile } */

struct X { int x; };

int test1(struct X x)
{
  if (x.x ==  1) return __builtin_fpclassify(1,2,3,4,5,x); /* { dg-error "non-floating-point argument" } */
  if (x.x ==  2) return __builtin_isfinite(x); /* { dg-error "non-floating-point argument" } */
  if (x.x ==  3) return __builtin_isinf_sign(x); /* { dg-error "non-floating-point argument" } */
  if (x.x ==  4) return __builtin_isinf(x); /* { dg-error "non-floating-point argument" } */
  if (x.x ==  5) return __builtin_isnan(x); /* { dg-error "non-floating-point argument" } */
  if (x.x ==  6) return __builtin_isnormal(x); /* { dg-error "non-floating-point argument" } */
  if (x.x ==  7) return __builtin_isgreater(x, x); /* { dg-error "non-floating-point arguments" } */
  if (x.x ==  8) return __builtin_isgreaterequal(x, x); /* { dg-error "non-floating-point arguments" } */
  if (x.x ==  9) return __builtin_isless(x, x); /* { dg-error "non-floating-point arguments" } */
  if (x.x == 10) return __builtin_islessequal(x, x); /* { dg-error "non-floating-point arguments" } */
  if (x.x == 11) return __builtin_islessgreater(x, x); /* { dg-error "non-floating-point arguments" } */
  if (x.x == 12) return __builtin_isunordered(x, x); /* { dg-error "non-floating-point arguments" } */

  return 0;
}

int test2(double x)
{
  if (x ==  1) return __builtin_fpclassify(1,2,3,4,5); /* { dg-error "not enough arguments" } */
  if (x ==  2) return __builtin_isfinite(); /* { dg-error "not enough arguments" } */
  if (x ==  3) return __builtin_isinf_sign(); /* { dg-error "not enough arguments" } */
  if (x ==  4) return __builtin_isinf(); /* { dg-error "not enough arguments" } */
  if (x ==  5) return __builtin_isnan(); /* { dg-error "not enough arguments" } */
  if (x ==  6) return __builtin_isnormal(); /* { dg-error "not enough arguments" } */
  if (x ==  7) return __builtin_isgreater(x); /* { dg-error "not enough arguments" } */
  if (x ==  8) return __builtin_isgreaterequal(x); /* { dg-error "not enough arguments" } */
  if (x ==  9) return __builtin_isless(x); /* { dg-error "not enough arguments" } */
  if (x == 10) return __builtin_islessequal(x); /* { dg-error "not enough arguments" } */
  if (x == 11) return __builtin_islessgreater(x); /* { dg-error "not enough arguments" } */
  if (x == 12) return __builtin_isunordered(x); /* { dg-error "not enough arguments" } */
  return 0;
}

int test3(double x)
{
  if (x ==  1) return __builtin_fpclassify(1,2,3,4,5,x,x); /* { dg-error "too many arguments" } */
  if (x ==  2) return __builtin_isfinite(x, x); /* { dg-error "too many arguments" } */
  if (x ==  3) return __builtin_isinf_sign(x, x); /* { dg-error "too many arguments" } */
  if (x ==  4) return __builtin_isinf(x, x); /* { dg-error "too many arguments" } */
  if (x ==  5) return __builtin_isnan(x, x); /* { dg-error "too many arguments" } */
  if (x ==  6) return __builtin_isnormal(x, x); /* { dg-error "too many arguments" } */
  if (x ==  7) return __builtin_isgreater(x, x, x); /* { dg-error "too many arguments" } */
  if (x ==  8) return __builtin_isgreaterequal(x, x, x); /* { dg-error "too many arguments" } */
  if (x ==  9) return __builtin_isless(x, x, x); /* { dg-error "too many arguments" } */
  if (x == 10) return __builtin_islessequal(x, x, x); /* { dg-error "too many arguments" } */
  if (x == 11) return __builtin_islessgreater(x, x, x); /* { dg-error "too many arguments" } */
  if (x == 12) return __builtin_isunordered(x, x, x); /* { dg-error "too many arguments" } */
  return 0;
}

int test4(int i, double x)
{
  if (x ==  1) return __builtin_fpclassify(i,2,3,4,5,x); /* { dg-error "non-const integer argument" } */
  if (x ==  2) return __builtin_fpclassify(1,i,3,4,5,x); /* { dg-error "non-const integer argument" } */
  if (x ==  3) return __builtin_fpclassify(1,2,i,4,5,x); /* { dg-error "non-const integer argument" } */
  if (x ==  4) return __builtin_fpclassify(1,2,3,i,5,x); /* { dg-error "non-const integer argument" } */
  if (x ==  5) return __builtin_fpclassify(1,2,3,4,i,x); /* { dg-error "non-const integer argument" } */
  return 0;
}
