// PR c++/84940
// { dg-additional-options -Wno-vla }

void
foo (int x)
{
  struct {} a[1][x](-a[0]); // { dg-error "wrong type argument to unary minus" }
}
