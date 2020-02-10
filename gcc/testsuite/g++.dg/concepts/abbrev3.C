// PR c++/69448
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

long x;

auto& f(auto) { return x; }
auto* g(auto) { return &x; }

long& r = f(1);
long* p = g(1);
