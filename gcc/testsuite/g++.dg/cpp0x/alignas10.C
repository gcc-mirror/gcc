// PR c++/79653
// { dg-do compile { target c++11 } }

template <typename... A> struct outer {
  template <typename... B> struct alignas(alignof(A) * alignof(B)...) inner {}; // { dg-error "mismatched argument pack lengths" }
};
outer<int>::inner<> mismatched_packs;
