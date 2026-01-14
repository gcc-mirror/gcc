// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_operator_function_template.

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
  operator T ();
};

bool operator&&(const S&, const S&);

template <typename T>
bool operator||(const S&, const T&);

int operator""_a(const char *);

template<char...>
int operator""_b();

constexpr auto conversion_template
  = (members_of (^^T, access_context::current ()) | std::views::filter (is_template)).front ();

static_assert (!is_operator_function_template (null_reflection));
static_assert (!is_operator_function_template (^^int));
static_assert (!is_operator_function_template (^^::));
static_assert (!is_operator_function_template (^^foo));
static_assert (!is_operator_function_template (^^S::operator+));
static_assert (!is_operator_function_template (^^operator&&));
static_assert (is_operator_function_template (^^operator||));
static_assert (!is_operator_function_template (^^operator||<int>));
static_assert (is_operator_function_template (^^S::operator-));
static_assert (!is_operator_function_template (^^S::operator-<int>));
static_assert (is_operator_function_template (template_of (^^S::operator-<int>)));
static_assert (!is_operator_function_template (^^S::operator int));
static_assert (!is_operator_function_template (conversion_template));
static_assert (!is_operator_function_template (^^S::fn));
static_assert (!is_operator_function_template (^^operator""_a));
static_assert (!is_operator_function_template (^^operator""_b));
