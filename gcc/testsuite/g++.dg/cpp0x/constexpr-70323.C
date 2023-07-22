// PR c++/70323
// { dg-do compile { target c++11 } }

constexpr int overflow_if_0 (int i) { return __INT_MAX__ + !i; }  // { dg-error "overflow in constant expression" }
constexpr int overflow_if_1 (int i) { return __INT_MAX__ + i; }   // { dg-error "overflow in constant expression" }

constexpr bool i0_0 = overflow_if_0 (0);   // { dg-message "in .constexpr. expansion of " }
constexpr bool i0_1 = overflow_if_0 (1);
constexpr bool i1_0 = overflow_if_1 (0);
constexpr bool i1_1 = overflow_if_1 (1);   // { dg-message "in .constexpr. expansion of " }
