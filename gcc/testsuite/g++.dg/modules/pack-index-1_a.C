// { dg-module-do run }
// { dg-additional-options { -std=c++26 -fmodules-ts } }

export module packing1;
// { dg-module-cmi "packing1" }

export template<int I, typename... Ts>
using Type = Ts...[I];

export template<int I, auto... Ts>
constexpr auto Var = Ts...[I];

export template <int I, auto...Ts>
int
foo ()
{
  return Ts...[I];
}
