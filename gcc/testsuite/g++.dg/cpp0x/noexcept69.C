// PR c++/100752
// { dg-do compile { target c++11 } }

struct S {
  void f() noexcept {}
  S &g() noexcept(noexcept(f())) { f(); return *this; }
};

struct X {
  int& f() noexcept(noexcept(i));
  int i;
};
