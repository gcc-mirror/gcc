// PR c++/70323
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

constexpr int overflow_if_0 (int i) { return __INT_MAX__ + !i; }
constexpr int overflow_if_1 (int i) { return __INT_MAX__ + i; }

constexpr bool i0_0 = overflow_if_0 (0);   // { dg-error "overflow in constant expression|in .constexpr. expansion of" }
constexpr bool i0_1 = overflow_if_0 (1);
constexpr bool i1_0 = overflow_if_1 (0);
constexpr bool i1_1 = overflow_if_1 (1);   // { dg-error "overflow in constant expression|in .constexpr. expansion of" }
