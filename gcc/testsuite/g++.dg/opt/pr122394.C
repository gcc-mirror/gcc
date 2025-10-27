// PR tree-optimization/122394
// { dg-do compile { target c++23 } }
// { dg-options "-O1 -g" }

#include <compare>

struct A {
  friend auto operator<=> (A, A) = default;
  double a;
};
void foo ();
A b, c;

void
bar ()
{
  bool d = c >= b;
  if (d)
    foo ();
}
