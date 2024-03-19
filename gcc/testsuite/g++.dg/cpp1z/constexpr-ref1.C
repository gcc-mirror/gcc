// P2280R4 - Using unknown pointers and references in constant expressions
// PR c++/106650
// { dg-do compile { target c++17 } }

#include <type_traits>

template <typename T, typename U>
constexpr bool is_type(U &&)
{
    return std::is_same_v<T, std::decay_t<U>>;
}

auto visitor = [](auto&& v) {
    if constexpr(is_type<int>(v)) {
        // ...
    } else if constexpr(is_type<char>(v)) {
        // ...
    }
};

void
g (int i)
{
  visitor (i);
  constexpr bool b = is_type<int>(i);
}
