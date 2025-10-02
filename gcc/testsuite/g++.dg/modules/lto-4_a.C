// PR c++/121865
// { dg-require-effective-target lto }
// { dg-additional-options "-fmodules -flto" }

export module M;
export template <typename T> struct S;
export template <typename T> void foo(S<T>) {}
template <typename T> struct S {
  friend void foo<>(S);
};
