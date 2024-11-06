// PR c++/88095
// Test class non-type template parameters for literal operator templates.
// Validate rejection of class template parameter packs.
// { dg-do compile { target c++20 } }

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

template <fixed_string...>
int operator ""_udl();     // { dg-error "5:literal operator template .int operator\"\"_udl\\(\\). has invalid parameter list" }
