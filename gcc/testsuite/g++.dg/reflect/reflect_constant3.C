// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant.

#include <meta>

using namespace std::meta;

static_assert ([:reflect_constant (42):] == 42);
static_assert (type_of (reflect_constant (42)) == ^^int);
static_assert (is_value (reflect_constant (42)));
static_assert (!is_object (reflect_constant (42)));

enum E { A = 42 };
static_assert ([:reflect_constant (A):] == A);
static_assert (type_of (reflect_constant (E::A)) == ^^E);
static_assert (is_value (reflect_constant (E::A)));
static_assert (!is_object (reflect_constant (E::A)));

enum class EC { A = 42 };
static_assert ([:reflect_constant (EC::A):] == EC::A);
static_assert (type_of (reflect_constant (EC::A)) == ^^EC);
static_assert (is_value (reflect_constant (EC::A)));
static_assert (!is_object (reflect_constant (EC::A)));

const int i = 42;
static_assert ([:reflect_constant (i):] == i);
static_assert (type_of (reflect_constant (i)) == ^^int);
static_assert (is_value (reflect_constant (i)));
static_assert (!is_object (reflect_constant (i)));

const int &r = 42;
static_assert ([:reflect_constant (r):] == r);
static_assert (type_of (reflect_constant (r)) == ^^int);
static_assert (is_value (reflect_constant (r)));
static_assert (!is_object (reflect_constant (r)));

constexpr int ci = 42;
static_assert ([:reflect_constant (ci):] == ci);
static_assert (type_of (reflect_constant (ci)) == ^^int);
static_assert (is_value (reflect_constant (ci)));
static_assert (!is_object (reflect_constant (ci)));

void fn() {}
static_assert ([:reflect_constant (&fn):] == &fn);
static_assert (type_of (reflect_constant (&fn)) == ^^void(*)());
static_assert (is_value (reflect_constant (&fn)));
static_assert (!is_object (reflect_constant (&fn)));

constexpr int cfn () { return 42; }
static_assert ([:reflect_constant (cfn ()):] == 42);
static_assert (type_of (reflect_constant (cfn ())) == ^^int);
static_assert (is_value (reflect_constant (cfn ())));
static_assert (!is_object (reflect_constant (cfn ())));

struct S {
  int k;
  void fn();
};
static_assert ([:reflect_constant (&S::k):] == &S::k);
static_assert (type_of (reflect_constant (&S::k)) == ^^int (S::*));
static_assert (is_value (reflect_constant (&S::k)));
static_assert (!is_object (reflect_constant (&S::k)));
static_assert ([:reflect_constant (&S::fn):] == &S::fn);
static_assert (type_of (reflect_constant (&S::fn)) == ^^void (S::*)());
static_assert (is_value (reflect_constant (&S::fn)));
static_assert (!is_object (reflect_constant (&S::fn)));
