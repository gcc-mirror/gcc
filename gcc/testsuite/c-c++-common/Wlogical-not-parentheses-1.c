/* PR c/77292 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-not-parentheses" } */

 /* Test that we don't warn if rhs is a comparison or a logical op.  */

int
foo (int a, int b)
{
  int r = 0;
  r += !a == (a < b);
  r += !a == (a > b);
  r += !a == (a >= b);
  r += !a == (a <= b);
  r += !a == (a != b);
  r += !a == (a == b);
  r += !a == (a || b);
  r += !a == (a && b);
  r += !a == (!b);

  r += !a == (a ^ b); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r += !a == (a | b); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  r += !a == (a & b); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */

  return r;
}
