// PR c++/115231
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;

template <typename T>
struct A {
  template <typename U> A(U);
};
template <typename T> A(T) -> A<T>;

export module M;

template <typename T>
struct B {
  template <typename U> B(U);
};
B(int) -> B<int>;

// Accessing deduction guides should be possible,
// even if we can't name the type directly.
export A<void> f();
export B<void> g();
