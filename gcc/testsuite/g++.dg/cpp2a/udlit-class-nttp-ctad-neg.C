// PR c++/88095
// Test class non-type template parameters for literal operator templates.
// Validate handling of failed class template argument deduction.
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
// Missing deduction guide.

template <fixed_string fs>
constexpr std::size_t operator"" _udl() {
  return decltype(fs)::length;
}

static_assert("test"_udl == 5); // { dg-error "15:no matching function for call to" }
                                // { dg-error "15:class template argument deduction failed" "" { target *-*-* } .-1 }
