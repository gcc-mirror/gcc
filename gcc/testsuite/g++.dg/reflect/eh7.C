// { dg-do compile { target c++26 } }
// { dg-require-iconv "IBM1047" }
// { dg-additional-options "-freflection -fexec-charset=IBM1047" }
// Test std::meta::exception.

#include <meta>

using namespace std::meta;

consteval bool
foo ()
{
  exception a (u8"This is a string", ^^foo);
  if (std::string_view (a.what ()) != std::string_view ("This is a string"))
    return false;
  exception b ("This is a string", ^^foo);
  if (b.u8what () != std::u8string_view (u8"This is a string"))
    return false;
  return true;
}

static_assert (foo ());
