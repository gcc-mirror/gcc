// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::type_of.

#include <meta>
using namespace std::meta;

template<typename T>
consteval auto
foo ()
{
  auto Functions = std::vector<info>{};
  for (auto Info : members_of (^^T, access_context::current ()))
    if (!is_special_member_function (Info) && is_function (Info))
      Functions.push_back(Info);
  return Functions;
}

struct F {
  auto f(int)->void;
};

void
g ()
{
  constexpr auto fInfo = foo<F>()[0];
  using fType = [:type_of(fInfo):];
  // TODO Should work: non-const non-volatile member functions have ordinary
  // function types.
  //static_assert (std::same_as<fType, auto(int)->void>);
}
