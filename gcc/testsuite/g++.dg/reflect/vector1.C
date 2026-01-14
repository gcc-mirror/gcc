// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::vector<std::meta::info>.

#include <vector>
#include <meta>

using namespace std::meta;

void die ();

consteval void
f ()
{
  constexpr auto e = std::vector<info>{^^int}[0];
  typename [:e:] i = 42;
  auto f = std::vector<info>{^^int}[0];
  std::vector<info> v1 = { ^^int };
  auto v2 = std::vector<info>{^^int};
  if (v1.size () != v2.size ())
    die ();
}

void
g ()
{
  f ();
}
