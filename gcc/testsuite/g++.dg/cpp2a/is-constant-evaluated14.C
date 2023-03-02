// PR c++/108243
// Verify a variable with static storage duration initialized with a
// constant initializer has constant initialization, and the initializer
// is manifestly constant-evaluated.
// { dg-do run { target c++11 } }
// { dg-additional-options "-fdump-tree-original" }

// { dg-final { scan-tree-dump-not "static initializers for" "original" } }
// { dg-final { scan-tree-dump-not "cxa_guard_acquire" "original" } }

#include <initializer_list>

struct A {
  constexpr A(int n) : n(n), m(__builtin_is_constant_evaluated()) { }
  constexpr A() : A(42) { }
  void verify_mce() const {
    if (m != 1) __builtin_abort();
  }
  int n;
  int m;
};

A a1 = {42};
A a2{42};
A a3(42);
A a4;
A a5{};

void f() {
  static A a1 = {42};
  static A a2{42};
  static A a3(42);
  static A a4;
  static A a5{};
  for (auto& a : {a1, a2, a3, a4, a5})
    a.verify_mce();
}

template<int... N>
void g() {
  static A a1 = {42};
  static A a2{42};
  static A a3(42);
  static A a4;
  static A a5{};
  static A a6 = {N...};
  static A a7{N...};
  static A a8(N...);
  for (auto& a : {a1, a2, a3, a4, a5, a6, a7, a8})
    a.verify_mce();
}

struct B {
  static A a1;
  static A a2;
  static A a3;
  static A a4;
  static A a5;
  static void verify_mce() {
    for (auto& a : {a1, a2, a3, a4, a5})
      a.verify_mce();
  }
};

A B::a1 = {42};
A B::a2{42};
A B::a3(42);
A B::a4;
A B::a5{};

template<int... N>
struct BT {
  static A a1;
  static A a2;
  static A a3;
  static A a4;
  static A a5;
  static A a6;
  static A a7;
  static A a8;
  static void verify_mce() {
    for (auto& a : {a1, a2, a3, a4, a5})
      a.verify_mce();
  }
};

template<int... N> A BT<N...>::a1 = {42};
template<int... N> A BT<N...>::a2{42};
template<int... N> A BT<N...>::a3(42);
template<int... N> A BT<N...>::a4;
template<int... N> A BT<N...>::a5{};
template<int... N> A BT<N...>::a6 = {N...};
template<int... N> A BT<N...>::a7{N...};
template<int... N> A BT<N...>::a8(N...);

#if __cpp_inline_variables
struct BI {
  static inline A a1 = {42};
  static inline A a2{42};
  static inline A a3;
  static inline A a4{};
  static void verify_mce() {
    for (auto& a : {a1, a2, a3, a4})
      a.verify_mce();
  }
};

template<int... N>
struct BIT {
  static inline A a1 = {42};
  static inline A a2{42};
  static inline A a3;
  static inline A a4{};
  static inline A a5 = {N...};
  static inline A a6{N...};
  static void verify_mce() {
    for (auto& a : {a1, a2, a3, a4, a5, a6})
      a.verify_mce();
  }
};
#endif

int main() {
  for (auto& a : {a1, a2, a3, a4, a5})
    a.verify_mce();

  f();
  g<42>();
  g<>();

  B::verify_mce();
  BT<42>::verify_mce();
  BT<>::verify_mce();

#if __cpp_inline_variables
  BI::verify_mce();
  BIT<42>::verify_mce();
  BIT<>::verify_mce();
#endif
}
