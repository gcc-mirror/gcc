// PR c++/125819
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <cstdlib>
#include <meta>

consteval bool
check ()
{
  bool found = false;
  for (auto m : members_of (^^::, std::meta::access_context::current ()))
    if (has_identifier (m))
      {
	if (identifier_of (m) == "__builtin_abs"
	    || identifier_of (m) == "__builtin_va_list")
	  return false;
	if (identifier_of (m) == "abs")
	  found = true;
      }
  return found;
}

static_assert (check ());
