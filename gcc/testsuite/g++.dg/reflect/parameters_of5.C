// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::parameters_of.

#include <meta>

using namespace std::meta;

struct C {
  ~C();
};

consteval void
f1 ()
{
  parameters_of (^^C);
}

consteval bool
test1 ()
{
  try { f1 (); }
  catch (std::meta::exception &) { return true; }
  catch (...) { return false; }
  return false;
}

consteval void
f2 ()
{
  parameters_of (^^int);
}

consteval bool
test2 ()
{
  try { f2 (); }
  catch (std::meta::exception &) { return true; }
  catch (...) { return false; }
  return false;
}

static_assert (test1 () && test2 ());
