// PR c++/126007
// { dg-do compile { target c++20 } }

template <bool B>
struct S {
  S () requires B = default;
  S (const S &) {}
};

static_assert (!__builtin_is_implicit_lifetime (S<false>));
static_assert (__builtin_is_implicit_lifetime (S<true>));
