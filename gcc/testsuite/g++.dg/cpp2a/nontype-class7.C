// Example from P0732.
// { dg-do compile { target c++20 } }

namespace std {
  using size_t = decltype(sizeof(1));
  template <typename CharT, std::size_t N>
  struct basic_fixed_string
  {
    constexpr basic_fixed_string(const CharT (&foo)[N+1])
    : m_data()
    {
      for (int i = 0; i <= N; ++i)
	m_data[i] = foo[i];
    }
    // auto operator<=>(const basic_fixed_string &) = default;
    CharT m_data[N+1];
  };
  template <typename CharT, std::size_t N>
  basic_fixed_string(const CharT (&str)[N])->basic_fixed_string<CharT, N-1>;
  template <std::size_t N>
  using fixed_string = basic_fixed_string<char, N>;
}

template <std::basic_fixed_string Str>
struct A {};
using hello_A = A<"hello">;
