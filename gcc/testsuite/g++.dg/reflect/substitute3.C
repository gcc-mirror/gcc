// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::substitute.  Test the case when undeduced_auto_decl
// is true after resolve_nondeduced_context.

#include <meta>

template<typename>
auto
f ()
{
}

consteval bool
g ()
{
  try { substitute (^^f, { ^^int }); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!g ());
