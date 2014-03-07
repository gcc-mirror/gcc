// PR c++/52637
// { dg-do compile { target c++11 } }
// { dg-options "-g" }

template <typename T>
struct C { };

template <typename V>
void f(V v) {
  struct B {};
  C<B> c;
}

template <typename T>
void g(T t) {
  struct A { } a;
  f (a);
}

struct D {
  void h() { g(0); }
};
