// PR c++/15640

struct A {
  void foo(void);
};

template <int> void bar() {
  A a;
  a + a.foo; // { dg-error "" }
}
