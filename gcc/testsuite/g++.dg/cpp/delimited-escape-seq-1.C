// P2290R3 - Delimited escape sequences
// { dg-do compile { target c++11 } }
// { dg-require-effective-target wchar }
// { dg-options "-pedantic" }

const char32_t *a = U"\u{1234}";	// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
const char32_t *b = U"\x{1234}";	// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
const char32_t *c = U"\o{1234}";	// { dg-warning "delimited escape sequences are only valid in" "" { target c++20_down } }
