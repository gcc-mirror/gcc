// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_constructor_template.

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
  template <typename U>
  T(int, const U &);
};

bool operator&&(const S&, const S&);

template <typename T>
bool operator||(const S&, const T&);

int operator""_a(const char *);

template<char...>
int operator""_b();

constexpr auto constructor_template
  = (members_of (^^T, access_context::current ()) | std::views::filter (is_template)).front ();

static_assert (!is_constructor_template (null_reflection));
static_assert (!is_constructor_template (^^int));
static_assert (!is_constructor_template (^^::));
static_assert (!is_constructor_template (^^foo));
static_assert (!is_constructor_template (^^S::operator+));
static_assert (!is_constructor_template (^^operator&&));
static_assert (!is_constructor_template (^^operator||));
static_assert (!is_constructor_template (^^operator||<int>));
static_assert (!is_constructor_template (^^S::operator-));
static_assert (!is_constructor_template (^^S::operator-<int>));
static_assert (!is_constructor_template (^^S::operator int));
static_assert (is_constructor_template (constructor_template));
static_assert (!is_constructor_template (^^S::fn));
static_assert (!is_constructor_template (^^operator""_a));
static_assert (!is_constructor_template (^^operator""_b));
