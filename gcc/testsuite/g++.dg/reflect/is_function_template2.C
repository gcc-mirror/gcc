// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_function_template.

#include <meta>

using namespace std::meta;

struct S {
  template<typename T>
  void ovl (T) { }

  template<typename T>
  void static_ovl (T) { }
};

static_assert (is_function_template (^^S::ovl));
static_assert (!is_function_template (^^S::ovl<int>));
static_assert (is_function_template (^^S::static_ovl));
static_assert (!is_function_template (^^S::static_ovl<int>));

static_assert (!is_function_template (^^void()));

template<info R1, info R2>
void
f ()
{
  static_assert (is_function_template (R1));
  static_assert (!is_function_template (R2));
}

template<typename T>
void
g ()
{
  f<^^S::ovl, ^^S::ovl<T>>();
  f<^^S::static_ovl, ^^S::static_ovl<T>>();
}

void
h ()
{
  g<int>();
}
