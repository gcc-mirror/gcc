// P2036R3 - Change scope of lambda trailing-return-type
// PR c++/102610
// { dg-do compile { target c++17 } }

template <typename T>
inline constexpr auto
equal1 (T &&t)
{
  return [t = 3](const auto& obj) -> decltype(obj == t)
    {
      return obj == t;
    };
}

template <typename T>
inline constexpr auto
equal2 (T &&t)
{
  return [t = t](const auto& obj) -> decltype(obj == t)
    {
      return obj == t;
    };
}

template <typename T>
inline constexpr auto
equal3 (T &&t)
{
  return [t = 4](const auto& obj) -> decltype(obj == t)
    {
      return obj == t;
    };
}

void
g ()
{
  constexpr auto l1 = equal1 (5);
  static_assert (l1 (3));
  constexpr auto l2 = equal2 (3);
  static_assert (l2 (3));
  constexpr auto l3 = equal3 (2);
  static_assert (l3 (4));
}
