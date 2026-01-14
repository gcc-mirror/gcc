// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_move_assignment.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
void foo ();

struct S {
  S ()
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
  }
  S (int)
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
  }
  S (const S &)
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
  }
  S (S &&)
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
  }
  S &operator = (const S &)
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
    return *this;
  }
  S &operator += (const S &)
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
    return *this;
  }
  void bar ()
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
  }
  ~S ()
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
  }
};

struct T {
  T &operator = (const S &)
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
    return *this;
  }
  T &operator *= (const S &)
  {
    int a;
    static_assert (!is_move_assignment (parent_of (^^a)));
    return *this;
  }
};

struct U {
  template <typename T>
  U (T &)
  {
    T a;
    static_assert (!is_move_assignment (parent_of (^^a)));
  }
  U &operator = (U &&)
  {
    int a;
    static_assert (is_move_assignment (parent_of (^^a)));
    return *this;
  }
};

struct V {
  V (int a = 42, long b = 18)
  {
    static_assert (!is_move_assignment (parent_of (^^a)));
  }
};

static_assert (!is_move_assignment (null_reflection));
static_assert (!is_move_assignment (^^int));
static_assert (!is_move_assignment (^^::));
static_assert (!is_move_assignment (^^foo));
static_assert (!is_move_assignment (^^S::bar));
static_assert (!is_move_assignment (^^S::operator =));
static_assert (!is_move_assignment (^^S::operator +=));
static_assert (!is_move_assignment (^^T::operator *=));
