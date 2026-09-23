// PR c++/125536
// { dg-do compile { target c++14 } }

template <typename>
constexpr bool x = false;

template <unsigned long long N>
constexpr bool x <char [N]> = true;

#if __PTRDIFF_MAX__ >= 2147483647
static_assert (x <char [2147483647]>, "");
#endif
#if __PTRDIFF_MAX__ > 2147483647
static_assert (x <char [2147483648U]>, "");
#endif
