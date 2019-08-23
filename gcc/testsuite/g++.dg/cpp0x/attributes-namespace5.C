// PR c++/79817 - attribute deprecated on namespace.
// { dg-do compile { target c++11 } }

namespace [[deprecated]] Y {
  void f();
  void f2(int);

  template<typename>
  struct S {
    void f3 ();
  };
}

void Y::f();
void Y::f() { }
void Y::f2(int);
void Y::f2([[maybe_unused]] int);
void Y::f2(int) { }
template <> void Y::S<int>::f3();
template <> void Y::S<int>::f3() { }
