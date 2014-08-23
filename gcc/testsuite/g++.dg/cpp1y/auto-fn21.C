// N3638: decltype(auto) must stand alone
// { dg-do compile { target c++14 } }

void f();
decltype(auto) g1() { return &f; }
decltype(auto)* g2() { return f; } // { dg-error "decltype.auto" }
