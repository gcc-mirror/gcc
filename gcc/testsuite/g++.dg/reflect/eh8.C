// { dg-do compile { target c++26 } }
// { dg-require-iconv "ISO-8859-2" }
// { dg-options "-freflection -fexec-charset=ISO-8859-2" }
// Test std::meta::exception.

#include <meta>

using namespace std::meta;

consteval bool
foo ()
{
  exception a (u8"h\u00e1\U0000010Dky a \u010d\u00E1rky", ^^foo);
  if (std::string_view (a.what ()) != std::string_view ("h\u00e1\U0000010Dky a \u010d\u00E1rky"))
    return false;
  exception b ("h\u00e1\U0000010Dky a \u010d\u00E1rky", ^^foo);
  if (b.u8what () != std::u8string_view (u8"h\u00e1\U0000010Dky a \u010d\u00E1rky"))
    return false;
  return true;
}

consteval bool
bar ()
{
  exception a (u8"\N{GRINNING FACE}\N{GRINNING FACE WITH SMILING EYES}\N{LEFT SPEECH BUBBLE}", ^^foo);
  const char *b = a.what ();	// { dg-message "in 'constexpr' expansion of 'a.std::meta::exception::what\\\(\\\)" }
  return true;			// { dg-error "inline assembly is not a constant expression" "" { target *-*-* } 0 }
}

static_assert (foo ());
constexpr auto c = bar ();
