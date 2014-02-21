// PR c++/60251
// { dg-options "-std=c++1y -pedantic-errors" }

void foo(int n)
{
  int x[n];
  [&x]() { decltype(x) y; }; // { dg-error "decltype of array of runtime bound" }
}
