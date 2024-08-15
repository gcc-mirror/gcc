// PR c++/116320
// { dg-do compile { target c++11 } }

template<class T> struct C;
template<class T> using C_ptr = C<T>*;

struct B { int m; using B_typedef = B; };

template<class T>
struct C : B {
  void f(C_ptr<T> c) {
    c->B::m;
    c->B_typedef::m;
  }
};

template struct C<int>;
