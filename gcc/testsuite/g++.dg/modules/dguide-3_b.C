// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi B }

export module B;

extern "C++" {
  template <typename T> struct S;
  S(int) -> S<int>;
  S(char) -> S<char>;
}
