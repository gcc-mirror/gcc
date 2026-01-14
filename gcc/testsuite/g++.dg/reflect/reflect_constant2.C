// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant.

#include <meta>

using namespace std::meta;

constexpr auto one = reflect_constant(1);
static_assert ([:one:] == 1);

template<info> struct S {};
template<info> void foo () {}

template<int P>
void
fn_tmpl ()
{
  static_assert(is_value(reflect_constant(P)));
  static_assert(!is_object(reflect_constant(P)));
  static_assert(type_of(reflect_constant(P)) == ^^int);
  static_assert([:reflect_constant(P):] == 1);
}

void
g ()
{
  S<reflect_constant (nullptr)> s;
  foo<reflect_constant (nullptr)>();
  fn_tmpl<1>();
}
