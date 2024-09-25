// PR c++/110065
// { dg-do compile { target c++17 } }
// { dg-additional-options -fconcepts }

template <typename>
inline constexpr bool t = false;

int
f ()
{
  return t<auto(&)(const int*) -> auto&>; // { dg-error "template argument" }
}

void
g ()
{
  t<auto(&)(const int*) -> auto&>; // { dg-error "template argument" }
}
