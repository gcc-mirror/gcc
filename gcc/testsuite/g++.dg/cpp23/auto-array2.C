// PR c++/101988
// { dg-do compile { target c++17 } }

template<class T> struct A { A(); };
A<int> a[3];
auto (*p)[3] = &a;
A<int> (*p2)[3] = &a;
A (*p3)[3] = &a; // { dg-error "template placeholder" }
auto (&r)[3] = a;
A<int> (&r2)[3] = a;
A (&r3)[3] = a; // { dg-error "template placeholder" }
