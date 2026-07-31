// PR c++/126508
// { dg-do compile { target c++26 } }

#include <exception>

constexpr bool
foo ()
{
  return __builtin_current_exception () != nullptr;
}

constexpr int
bar ()
{
  return __builtin_current_exception () != nullptr;
}

constexpr bool
baz ()
{
  if (foo ())
    return false;
  try
    {
      throw 42;
    }
  catch (...)
    {
      if (!foo ())
	return false;
      if (!bar ())
	return false;
    }
  if (foo ())
    return false;
  if (bar ())
    return false;
  return true;
}

static_assert (baz ());
