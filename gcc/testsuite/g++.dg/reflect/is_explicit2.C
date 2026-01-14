// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_explicit.

#include <meta>
using namespace std::meta;

template<typename Fn>
consteval info select(info clazz, Fn f)
{
  for (auto x : members_of(clazz, access_context::unchecked()))
   if (f(x))
     return x;
}

struct Implicit {
  template<typename T>
  Implicit(T&& t);

  template<typename T>
  operator T();
};

static_assert (!is_explicit (select(^^Implicit, is_constructor_template)));
static_assert (!is_explicit (select(^^Implicit, is_conversion_function_template)));

struct Explicit {
  template<typename T>
  explicit Explicit(T&& t);

  template<typename T>
  explicit operator T();
};

static_assert (!is_explicit (select(^^Explicit, is_constructor_template)));
static_assert (!is_explicit (select(^^Explicit, is_conversion_function_template)));

struct CondExplicit {
  template<typename T>
  explicit(sizeof(T) > 4) CondExplicit(T&& t);

  template<typename T>
  explicit(sizeof(T) > 4) operator T();
};

static_assert (!is_explicit (select(^^CondExplicit, is_constructor_template)));
static_assert (!is_explicit (select(^^CondExplicit, is_conversion_function_template)));
