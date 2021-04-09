// { dg-do compile { target c++23 } }

#include <cstddef>
#include <type_traits>

static_assert(std::is_same_v<decltype(123zu), std::size_t>);
static_assert(std::is_same_v<decltype(456z), std::make_signed_t<std::size_t>>);

