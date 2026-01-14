// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using namespace std::meta;

template <typename>
void
foo ()
{
  constexpr int i = [: reflect_constant (5) :];
  constexpr info f = reflect_constant_array (std::vector { 1, 2, 3 });
  constexpr auto &arr = [: f :];
  constexpr auto &arr2 = [: reflect_constant_array (std::vector { 1, 2, 3 }) :];
}

void
bar ()
{
  constexpr auto &arr = [: reflect_constant_array (std::vector { 1, 2, 3 }) :];   
}

int
main ()
{
  foo <int> ();
  bar ();
}
