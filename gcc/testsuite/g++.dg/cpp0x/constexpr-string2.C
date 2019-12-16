// PR c++/91165
// { dg-do compile { target c++11 } }
// { dg-additional-options -O }

template <typename T> constexpr T bar (T c) { return c; }
template <typename T, typename U> struct S {
  T f;
  U g;
};
template <typename T, typename U>
constexpr S<T, U> foo (T &&c, U h) { return S<T, U> {c, bar (h)}; }
void baz (int a) { foo (a, ""); }
void qux () { foo (0, ""); }
