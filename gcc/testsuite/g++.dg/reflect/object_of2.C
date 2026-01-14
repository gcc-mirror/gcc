// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::object_of.

#include <meta>

using namespace std::meta;

void fn ();
constexpr auto r1 = object_of (reflect_constant (3)); // { dg-error "uncaught exception" }
constexpr auto r2 = object_of (^^fn); // { dg-error "uncaught exception" }

static int G;

void
f (int p)
{
  int i = 42;
  constexpr auto r3 = object_of (^^p); // { dg-error "uncaught exception" }
  constexpr auto r4 = object_of (^^i); // { dg-error "uncaught exception" }
}

template<int>
struct X { };
constexpr auto T = ^^X<1>;
constexpr auto o = object_of (template_arguments_of (T)[0]); // { dg-error "uncaught exception" }
