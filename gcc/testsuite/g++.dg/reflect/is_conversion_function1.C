// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_conversion_function.

#include <meta>
#include <ranges>

using namespace std::meta;

constexpr info null_reflection;
void foo ();

struct S {
  S &operator+(const S&);

  template <typename T>
  S &operator-(const S&);

  operator int();

  void fn();
};

struct T {
  template <typename T>
  operator T();
};

bool operator&&(const S&, const S&);

template <typename T>
bool operator||(const S&, const T&);

int operator""_a(const char *);

template<char...>
int operator""_b();

constexpr auto conversion_template
  = (members_of (^^T, access_context::current ()) | std::views::filter (is_template)).front ();

static_assert (!is_conversion_function (null_reflection));
static_assert (!is_conversion_function (^^int));
static_assert (!is_conversion_function (^^::));
static_assert (!is_conversion_function (^^foo));
static_assert (!is_conversion_function (^^S::operator+));
static_assert (!is_conversion_function (^^operator&&));
static_assert (!is_conversion_function (^^operator||));
static_assert (!is_conversion_function (^^operator||<int>));
static_assert (!is_conversion_function (^^S::operator-));
static_assert (!is_conversion_function (^^S::operator-<int>));
static_assert (is_conversion_function (^^S::operator int));
static_assert (!is_conversion_function (conversion_template));
static_assert (!is_conversion_function (^^S::fn));
static_assert (!is_conversion_function (^^operator""_a));
static_assert (!is_conversion_function (^^operator""_b));
