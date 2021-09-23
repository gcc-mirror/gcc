// PR c++/93867
// { dg-do compile { target c++20 } }

template <typename CharT, unsigned N>
struct basic_fixed_string
{
  constexpr basic_fixed_string(const CharT *p) {
    for (int i = 0; i < N; ++i) {
      m_data[i] = p[i];
    }
  }

  CharT m_data[N] {};
};

template <typename CharT, unsigned N>
basic_fixed_string(const CharT (&)[N]) -> basic_fixed_string<CharT,N>;

template <unsigned N>
using fixed_string = basic_fixed_string<char, N>;

template <fixed_string path>
constexpr int foo()
{
  return 42;
}

int main(int argc, char const *argv[])
{
  foo<"hello">();
  return 0;
}
