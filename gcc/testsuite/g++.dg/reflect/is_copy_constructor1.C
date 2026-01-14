// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_copy_constructor.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
void foo ();

struct S {
  S ()
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
  }
  S (int)
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
  }
  S (const S &)
  {
    int a;
    static_assert (is_copy_constructor (parent_of (^^a)));
  }
  S (S &&)
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
  }
  S &operator = (const S &)
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
    return *this;
  }
  S &operator += (const S &)
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
    return *this;
  }
  void bar ()
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
  }
  ~S ()
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
  }
};

struct T {
  T &operator = (const S &)
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
    return *this;
  }
  T &operator *= (const S &)
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
    return *this;
  }
};

struct U {
  template <typename T>
  U (T &)
  {
    T a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
  }
  U &operator = (U &&)
  {
    int a;
    static_assert (!is_copy_constructor (parent_of (^^a)));
    return *this;
  }
};

struct V {
  V (int a = 42, long b = 18)
  {
    static_assert (!is_copy_constructor (parent_of (^^a)));
  }
};

static_assert (!is_copy_constructor (null_reflection));
static_assert (!is_copy_constructor (^^int));
static_assert (!is_copy_constructor (^^::));
static_assert (!is_copy_constructor (^^foo));
static_assert (!is_copy_constructor (^^S::bar));
static_assert (!is_copy_constructor (^^S::operator =));
static_assert (!is_copy_constructor (^^S::operator +=));
static_assert (!is_copy_constructor (^^T::operator *=));
