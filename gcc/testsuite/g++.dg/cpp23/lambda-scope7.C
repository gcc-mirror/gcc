// P2036R3 - Change scope of lambda trailing-return-type
// PR c++/102610
// { dg-do compile { target c++14_only } }

template <typename T>
inline constexpr auto
equal1 (T &&t)
{
  return [t = 3](const auto& obj) -> decltype(obj == t)
    {
      return obj == t;
    };
}

void
g ()
{
  constexpr auto l1 = equal1 (5); // { dg-error "not literal" }
  static_assert (l1 (3), "");	  // { dg-error "non-constant|non-.constexpr." }
}
