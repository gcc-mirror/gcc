// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::template_of.

#include <meta>

consteval void
foo ()
{
  template_of (^^int);
}

consteval bool
test ()
{
  try { foo (); }
  catch (std::meta::exception &) { return true; }
  catch (...) { return false; }
  return false;
}

static_assert (test ());
