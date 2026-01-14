// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test throwing std::meta::exception.

#include <meta>

consteval void
foo ()
{
  throw std::meta::exception{"foo", ^^int};
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
