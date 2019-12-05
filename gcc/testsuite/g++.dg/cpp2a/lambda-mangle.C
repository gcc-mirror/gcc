// Test lambda mangling
// { dg-do compile { target c++2a } }
// { dg-require-weak "" }
// { dg-options "-fno-inline" }

template<typename T> struct R {
  static int x;
};
// "int i;" makes the op() non-constexpr in C++17.  In C++20, it does not.
template<typename T> int R<T>::x = []{int i; return 1;}();
template int R<int>::x;
// Type of lambda in intializer of R<int>::x: N1RIiE1xMUlvE_E
// Corresponding operator(): _ZNK1RIiE1xMUlvE_clEv
// { dg-final { scan-assembler-not "_ZNK1RIiE1xMUlvE_clEv" } }
// { dg-final { scan-assembler-not "weak\[^\n\r\]*_?_ZNK1RIiE1xMUlvE_clEv" } }
