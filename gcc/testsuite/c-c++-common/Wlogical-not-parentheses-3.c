/* PR c/77423 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-not-parentheses" } */

#ifndef __cplusplus
# define bool _Bool
#endif

int
f (int a, bool b, bool c)
{
  int r = 0;

  r += !a == (b | c);
  r += !a == (b ^ c);
  r += !a == (b & c);
  r += !a == ~b;
  r += !a == ~(int) b;
  r += !a == ((b & c) | c);
  r += !a == ((b & c) | (b ^ c));
  r += !a == (int) (b ^ c);
  r += !a == (int) ~b;
  r += !a == ~~b;
  r += !a == ~(b | c);
  r += !a == ~(b | (a == 1));
  r += !a == ~(a == 1);

  r += !a == ((b & c) | (b ^ a)); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */

  return r;
}
