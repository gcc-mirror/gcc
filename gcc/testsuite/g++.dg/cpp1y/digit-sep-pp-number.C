// Test lexing of pp-numbers does not allow digit separators that do
// not form part of the pp-number syntax, when the code is valid with
// correct lexing but not with too many characters accepted in the
// pp-number (bug 97604).
// { dg-do compile { target c++14 } }

static_assert (0x0'e-0xe == 0, "signs");

#define a0 '.' -
#define acat(x) a ## x
static_assert (acat (0'.') == 0, ".");

// This case was not actually buggy.
#define c0(x) 0
#define b0 c0 (
#define bcat(x) b ## x
static_assert (bcat (0'\u00c0')) == 0, "identifier-nondigit");
