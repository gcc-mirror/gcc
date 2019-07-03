// PR c++/86476 - noexcept-specifier is a complete-class context
// { dg-do compile { target c++11 } }

int g;

struct S {
  int b;
  friend void fn1(int n) noexcept(noexcept(n));
  friend void fn2() noexcept(noexcept(g));
  friend void fn3() noexcept(noexcept(b));
};
