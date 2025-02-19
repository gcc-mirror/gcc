// PR c++/118846
// { dg-additional-options "-fmodules" }

module M;

template <typename> struct S {
  template <typename> struct fn {};
};
static_assert(mp_count_if_impl<S>::type::value == 0);
static_assert(use<S<int>>::type::value);
