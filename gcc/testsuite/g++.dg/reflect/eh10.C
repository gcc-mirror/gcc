// { dg-do compile { target c++26 } }
// { dg-require-iconv "ISO-8859-2" }
// { dg-options "-freflection -fexec-charset=ISO-8859-2" }
// { dg-skip-if "PR c++/124118" *-*-solaris2* }
// Test std::meta::exception.

#include <meta>

using namespace std::meta;

consteval bool
bar ()
{
  exception a (u8"\N{GRINNING FACE}\N{GRINNING FACE WITH SMILING EYES}\N{LEFT SPEECH BUBBLE}", ^^bar);
  const char *b = a.what ();	// { dg-message "in 'constexpr' expansion of 'a.std::meta::exception::what\\\(\\\)" }
  return true;			// { dg-error "constexpr message: std::meta::exception message could not be successfully transcoded from UTF-8 to ordinary literal encoding" "" { target *-*-* } 0 }
}

constexpr auto c = bar ();
