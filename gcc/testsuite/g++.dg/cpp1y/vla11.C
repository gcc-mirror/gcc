// PR c++/60251
// { dg-do compile { target c++1y } }

void foo(int n)
{
  int x[n];
  [&x]() { decltype(x) y; }; // { dg-error "decltype of array of runtime bound" }
}
