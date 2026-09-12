// C++26 P3598R0 - CWG 3158 - const-ification of Splice Expressions
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <array>
#include <meta>
#include <vector>

template <typename T1, typename T2>
auto
foo (const T1 &t1, const T2 &t2)
  pre([: is_integral_type (^^T1) ? ^^t2 : ^^t1 :]
      [ [: is_integral_type (^^T1) ? ^^t1 : ^^t2 :] ] >= 0)
{
  constexpr auto key = is_integral_type (^^T1) ? ^^t1 : ^^t2;
  constexpr auto container = is_integral_type (^^T1) ? ^^t2 : ^^t1;
  return [: container :][ [: key :] ];
}

void
bar (std::vector <int> &v)
{
  foo (v, 5);
  foo (5, v);
}

template <std::size_t N>
void
baz (std::array <int, N> &v)
{
  foo (v, 5);
  foo (5, v);
}

void
qux (std::array <int, 7> &v)
{
  baz (v);
}
