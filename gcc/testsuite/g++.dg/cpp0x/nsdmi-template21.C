// PR c++/101532
// { dg-do compile { target c++11 } }

struct A { private: ~A(); };

template<class> struct B { A a = A(); }; // { dg-error "private" }

B<int> b; // { dg-error "deleted" }
