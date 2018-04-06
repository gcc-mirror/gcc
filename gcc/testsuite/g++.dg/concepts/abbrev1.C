// PR c++/64969
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

auto f1(auto x) { return *x; }
decltype(auto) f2(auto x) { return *x; }
auto f3(auto x) -> int { return *x; }

int i;
auto r1 = f1(&i);
auto r2 = f2(&i);
auto r3 = f3(&i);
