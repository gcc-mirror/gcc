// PR c++/111455
// { dg-do compile { target c++11 } }

namespace std
{
  template <typename T, T... I>
  struct integer_sequence {};

  template <typename T, T N>
  using make_integer_sequence
    = integer_sequence <T, __integer_pack (N)...>;
}

template <long... V>
void foo (std::integer_sequence <long, V...>)
{}

template <typename ...T>
struct U
{
  static constexpr long value = 1;
  constexpr operator int () = delete;
  constexpr operator long () { return value; }
};

template <typename T>
struct R
{
  using S = std::make_integer_sequence <long, U <T> {}>;
  R () noexcept (noexcept (foo (S ()))) {}
};

int
main ()
{
  R <long>();
}
