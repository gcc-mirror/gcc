/* { dg-do compile } */
/* { dg-options "-gdwarf-4" } */

/* Regression test for an ICE in output_die when using -gdwarf-4.  */

namespace {

struct A {
  virtual ~A();
};

struct B : public A {
  template <typename A>
  bool foo(A x[2]) { }
};

template <typename T>
struct C {
  T v[2];
};

template <typename T>
bool X(T &b) {
  typedef C<int> D;
  D x[2];
  return b.foo(x);
}

void f() {
  B b;
  X<B>(b);
}

}
