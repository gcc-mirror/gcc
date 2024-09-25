// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi A }

export module A;

extern "C++" {
  template <typename T> struct S;
  S(int) -> S<int>;
  S(double) -> S<double>;
}
