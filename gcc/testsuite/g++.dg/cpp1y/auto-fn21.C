// N3638: decltype(auto) must stand alone
// { dg-options "-std=c++1y" }

void f();
decltype(auto) g1() { return &f; }
decltype(auto)* g2() { return f; } // { dg-error "decltype.auto" }
