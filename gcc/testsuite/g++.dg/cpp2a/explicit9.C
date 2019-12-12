// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a -fconcepts" }

#include <type_traits>

template <typename T1, typename T2>
struct pair {
    template <typename U1=T1, typename U2=T2>
        requires std::is_constructible_v<T1, U1> &&
            std::is_constructible_v<T2, U2>
    explicit(!std::is_convertible_v<U1, T1> ||
        !std::is_convertible_v<U2, T2>)
    constexpr pair(U1&&, U2&&) { }
};

void
foo ()
{
  pair<int, int> p{1, 2};
  pair<int, int> p2 = {1, 2};
}
