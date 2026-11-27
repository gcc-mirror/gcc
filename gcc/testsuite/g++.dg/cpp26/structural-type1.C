// C++26 P3861R1 - Pragmatic approach to standard structural types
// { dg-do compile { target c++26 } }

#include <optional>
#include <span>
#include <string_view>
#include <tuple>
#include <variant>

struct S { int s; };
struct T { constexpr T () : t (0) {} private: int t; };
static_assert (__builtin_is_structural (std::string_view));
static_assert (__builtin_is_structural (std::basic_string_view <char>));
static_assert (__builtin_is_structural (std::basic_string_view <wchar_t>));
static_assert (__builtin_is_structural (std::span <int>));
static_assert (__builtin_is_structural (std::span <int, 6>));
static_assert (__builtin_is_structural (std::tuple <>));
static_assert (__builtin_is_structural (std::tuple <int, long>));
static_assert (__builtin_is_structural (std::tuple <S>));
static_assert (!__builtin_is_structural (std::tuple <T>));
static_assert (__builtin_is_structural (std::optional <int>));
static_assert (__builtin_is_structural (std::optional <S>));
static_assert (!__builtin_is_structural (std::optional <T>));
static_assert (__builtin_is_structural (std::variant <S>));
static_assert (__builtin_is_structural (std::variant <int, long>));
static_assert (!__builtin_is_structural (std::variant <T>));
