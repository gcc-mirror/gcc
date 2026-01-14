// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_special_member_function.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
void foo ();

struct S {
  S ()
  {
    int a;
    static_assert (is_special_member_function (parent_of (^^a)));
  }
  S (int)
  {
    int a;
    static_assert (!is_special_member_function (parent_of (^^a)));
  }
  S (const S &)
  {
    int a;
    static_assert (is_special_member_function (parent_of (^^a)));
  }
  S (S &&)
  {
    int a;
    static_assert (is_special_member_function (parent_of (^^a)));
  }
  S &operator = (const S &)
  {
    int a;
    static_assert (is_special_member_function (parent_of (^^a)));
    return *this;
  }
  S &operator += (const S &)
  {
    int a;
    static_assert (!is_special_member_function (parent_of (^^a)));
    return *this;
  }
  void bar ()
  {
    int a;
    static_assert (!is_special_member_function (parent_of (^^a)));
  }
  ~S ()
  {
    int a;
    static_assert (is_special_member_function (parent_of (^^a)));
  }
};

struct T {
  T &operator = (const S &)
  {
    int a;
    static_assert (!is_special_member_function (parent_of (^^a)));
    return *this;
  }
  T &operator *= (const S &)
  {
    int a;
    static_assert (!is_special_member_function (parent_of (^^a)));
    return *this;
  }
};

struct U {
  template <typename T>
  U (T &)
  {
    T a;
    static_assert (is_constructor (parent_of (^^a)));
  }
  U &operator = (U &&)
  {
    int a;
    static_assert (is_special_member_function (parent_of (^^a)));
    return *this;
  }
};

struct V {
  V (int a = 42, long b = 18)
  {
    static_assert (is_special_member_function (parent_of (^^a)));
  }
};

static_assert (!is_special_member_function (null_reflection));
static_assert (!is_special_member_function (^^int));
static_assert (!is_special_member_function (^^::));
static_assert (!is_special_member_function (^^foo));
static_assert (!is_special_member_function (^^S::bar));
static_assert (is_special_member_function (^^S::operator =));
static_assert (!is_special_member_function (^^S::operator +=));
static_assert (!is_special_member_function (^^T::operator *=));
