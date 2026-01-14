// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::access_context.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
struct S {};

constexpr access_context a = access_context::unprivileged ();
static_assert (a.scope () == ^^:: && a.designating_class () == null_reflection);
constexpr access_context b = access_context::unchecked ();
static_assert (b.scope () == null_reflection && b.designating_class () == null_reflection);
constexpr access_context c = access_context::current ();
static_assert (c.scope () == ^^:: && c.designating_class () == null_reflection);
constexpr access_context d = access_context::unprivileged ().via (^^S);
static_assert (d.scope () == ^^:: && d.designating_class () == ^^S);
constexpr access_context e = access_context::unchecked ().via (^^const S);
static_assert (e.scope () == null_reflection && e.designating_class () == ^^const S);
constexpr access_context f = access_context::current ().via (^^volatile S);
static_assert (f.scope () == ^^:: && f.designating_class () == ^^volatile S);

consteval info
foo (info x = access_context::current ().scope ())
{
  static_assert (access_context::current ().scope () == ^^foo);
  return x;
}

static_assert (foo (^^std) == ^^std);
static_assert (foo () == ^^::);

namespace N
{
  struct S {};
  constexpr access_context a = access_context::current ().via (^^::S).via (^^S);
  static_assert (a.scope () == ^^N && a.designating_class () == ^^S);
  static_assert (access_context::unprivileged ().scope () == ^^::);
  static_assert (access_context::unchecked ().scope () == null_reflection);
  namespace M
  {
    constexpr access_context a = access_context::current ().via (^^S).via (^^::S);
    static_assert (a.scope () == ^^M && a.designating_class () == ^^::S);
    static_assert (foo () == ^^M);
    consteval {
      static_assert (access_context::current ().scope () == ^^M);
      consteval {
        static_assert (access_context::current ().scope () == ^^M);
      }
    }
  }
}

struct T
{
  static_assert (access_context::unprivileged ().scope () == ^^::);
  static_assert (access_context::unchecked ().scope () == null_reflection);
  static_assert (access_context::current ().scope () == ^^T);
  static_assert (access_context::unprivileged ().designating_class () == null_reflection);
  static_assert (access_context::unchecked ().designating_class () == null_reflection);
  static_assert (access_context::current ().designating_class () == null_reflection);
  static_assert (foo () == ^^T);
  info t = access_context::current ().scope ();
};

struct U
{
  consteval U () {}
  info u = access_context::current ().scope ();
};

struct V : U
{
  using U::U;
  info v = access_context::current ().scope ();
  consteval {
    static_assert (access_context::current ().scope () == ^^V);
    consteval {
      static_assert (access_context::current ().scope () == ^^V);
    }
  }
};

void
bar ()
{
  static constexpr access_context a = access_context::current ();
  static_assert (a.scope () == ^^bar && a.designating_class () == null_reflection);
  static_assert (foo (^^foo) == ^^foo);
  static_assert (foo () == ^^bar);
  static_assert (T {}.t == ^^bar);
  consteval {
    static_assert (access_context::current ().scope () == ^^bar);
    consteval {
      static_assert (access_context::current ().scope () == ^^bar);
    }
  }
  auto l = [] () {
    int v = 0;
    static_assert (access_context::current ().scope () == parent_of (^^v));
    static_assert (parent_of (parent_of (access_context::current ().scope ())) == ^^bar);
  };
  auto l2 = [] () -> int (&) [access_context::current ().scope () == ^^bar ? 2 : 3] {
    static int a[2];
    return a;
  };
}

consteval {
  static_assert (access_context::current ().scope () == ^^::);
  consteval {
    static_assert (access_context::current ().scope () == ^^::);
  }
  static_assert (access_context::current ().via (^^S).via (null_reflection).designating_class () == null_reflection);
}

consteval bool
baz ()
{
  constexpr U u;
  static_assert (is_constructor (u.u) && parent_of (u.u) == ^^U);
  constexpr V v;
  static_assert (is_constructor (v.u) && parent_of (v.u) == ^^U);
  static_assert (is_constructor (v.v) && parent_of (v.v) == ^^V);
  return true;
}

static_assert (baz ());

consteval info
qux ()
{
  return ^^qux;
}

constexpr info q = qux ();
static_assert (q == ^^qux);

namespace O
{
  int a[2];
  auto
  foo () -> int (&) [access_context::current ().scope () == ^^O ? 2 : 3]
  {
    return a;
  }
}

consteval bool
can_do_via (info r)
{
  try { access_context::current ().via (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

struct W;

static_assert (can_do_via (^^S));
static_assert (can_do_via (^^T));
static_assert (can_do_via (^^U));
static_assert (can_do_via (^^V));
static_assert (can_do_via (^^N::S));
static_assert (can_do_via (null_reflection));
static_assert (!can_do_via (^^W));
static_assert (!can_do_via (^^::));
static_assert (!can_do_via (^^O));
static_assert (!can_do_via (^^bar));
static_assert (!can_do_via (^^int));
enum E { E0, E1 };
static_assert (!can_do_via (^^E));
static_assert (!can_do_via (^^E0));
static_assert (!can_do_via (reflect_constant (42)));
