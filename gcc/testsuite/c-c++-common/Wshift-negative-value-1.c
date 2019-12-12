/* PR c/65179 */
/* { dg-do compile } */
/* { dg-options "-O -Wextra" } */
/* { dg-additional-options "-std=c++11" { target c++ } } */

enum E {
  A = 0 << 1,
  B = 1 << 1,
  C = -1 << 1, /* { dg-warning "10:left shift of negative value|not an integer constant" } */
  /* { dg-error "left operand of shift expression" "shift" { target c++ } .-1 } */
  D = 0 >> 1,
  E = 1 >> 1,
  F = -1 >> 1
};

int
left (int x)
{
  /* Warn for LSHIFT_EXPR.  */
  const int z = 0;
  const int o = 1;
  const int m = -1;
  int r = 0;
  r += z << x;
  r += o << x;
  r += m << x; /* { dg-warning "10:left shift of negative value" } */
  r += 0 << x;
  r += 1 << x;
  r += -1 << x; /* { dg-warning "11:left shift of negative value" } */
  r += -1U << x;
  return r;
}

int
right (int x)
{
  /* Shouldn't warn for RSHIFT_EXPR.  */
  const int z = 0;
  const int o = 1;
  const int m = -1;
  int r = 0;
  r += z >> x;
  r += o >> x;
  r += m >> x;
  r += 0 >> x;
  r += 1 >> x;
  r += -1 >> x;
  r += -1U >> x;
  return r;
}
