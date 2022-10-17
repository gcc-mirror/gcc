// PR c++/101725
// { dg-do compile { target c++20 } }

template<class T, bool V = requires (T t) { x(t); }> void f();

struct A {
  int m;
  void f(int a, int b = requires (int t) { a + m + t; });
};

void g();
static_assert(noexcept(requires { g(); }));
