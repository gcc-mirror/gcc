// PR c++/95870
// { dg-do compile { target c++11 } }

template <typename> struct S {
  S();
  int b = []() -> int { enum E {}; return 1; }();
};
struct C : S<int> {
  C();
};
template <typename T> S<T>::S() = default;
C::C() {}
