// PR c++/24560

struct A { void f(); };
void g() { A().f.a; } // { dg-error "invalid use of member function" }
