// PR c++/51619
// { dg-do compile { target c++11 } }

struct A { virtual ~A(); };
struct B { A a[1][1]; } b;
struct C { A a[3][3]; } c;
