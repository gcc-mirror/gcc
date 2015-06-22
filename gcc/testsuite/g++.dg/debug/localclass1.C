// PR c++/52637
// { dg-options "-g -std=c++11" }

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
