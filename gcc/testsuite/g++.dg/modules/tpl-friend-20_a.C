// PR c++/122551
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;
export template <typename T> struct S {
  operator int() const { return 123; }
  template <typename U> friend void f(U);
  template <typename U> friend void g(U) {}
  friend void h(int);
  friend void i(int) {}
};
template <typename U> void f(U) {}
template <typename U> void g(U);
void h(int) {}
void i(int);
