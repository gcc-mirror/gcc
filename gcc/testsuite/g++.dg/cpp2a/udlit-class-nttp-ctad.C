// PR c++/88095
// Test class non-type template parameters for literal operator templates.
// Validate support for class template argument deduction.
// { dg-do compile { target c++2a } }

namespace std {
using size_t = decltype(sizeof(int));
}

template <typename CharT, std::size_t N>
struct fixed_string {
  constexpr static std::size_t length = N;
  constexpr fixed_string(...) { }
  // auto operator<=> (const fixed_string&) = default;
};
template <typename CharT, std::size_t N>
fixed_string(const CharT (&str)[N]) -> fixed_string<CharT, N>;

template <fixed_string fs>
constexpr std::size_t operator"" _udl() {
  return decltype(fs)::length;
}

static_assert("test"_udl == 5);
