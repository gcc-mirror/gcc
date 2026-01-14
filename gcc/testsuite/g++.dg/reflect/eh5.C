// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test throwing std::meta::exception.

#include <meta>

using namespace std::meta;

template <int N>
struct S {};

consteval bool
foo (info x)
{
  try
    {
      is_class_type (x);
    }
  catch (std::meta::exception &ex)
    {
      std::string_view what = ex.what ();
      info from = ex.from ();
      std::source_location loc = ex.where ();
      if (loc.line () != std::source_location::current ().line () - 7
	  || from != ^^std::meta::is_class_type
	  || what != "reflection does not represent a type"
	  || ex.u8what () != u8"reflection does not represent a type")
	return false;
      return true;
    }
  return false;
}

consteval bool
bar (info x)
{
  try
    {
      parameters_of (x);
    }
  catch (std::meta::exception &ex)
    {
      std::string_view what = ex.what ();
      info from = ex.from ();
      std::source_location loc = ex.where ();
      if (loc.line () != std::source_location::current ().line () - 7
	  || from != ^^std::meta::parameters_of
	  || what != "reflection does not represent a function or function type"
	  || ex.u8what () != u8"reflection does not represent a function or function type")
	return false;
      return true;
    }
  return false;
}

consteval bool
baz (info x)
{
  try
    {
      data_member_spec (x, { .name = "consteval" });
    }
  catch (std::meta::exception &ex)
    {
      std::string_view what = ex.what ();
      info from = ex.from ();
      std::source_location loc = ex.where ();
      if (loc.line () != std::source_location::current ().line () - 7
	  || from != ^^std::meta::data_member_spec
	  || what != "name is a keyword"
	  || ex.u8what () != u8"name is a keyword")
	return false;
      return true;
    }
  return false;
}

consteval bool
qux (info x)
{
  try
    {
      can_substitute (^^S, { x });
    }
  catch (std::meta::exception &ex)
    {
      std::string_view what = ex.what ();
      info from = ex.from ();
      std::source_location loc = ex.where ();
      if (loc.line () != std::source_location::current ().line () - 7
	  || from != ^^std::meta::can_substitute
	  || what != "invalid argument to can_substitute"
	  || ex.u8what () != u8"invalid argument to can_substitute")
	return false;
      return true;
    }
  return false;
}

static_assert (foo (^^::));
static_assert (bar (^^::));
static_assert (baz (^^int));
static_assert (qux (^^::));
