// P2071R2 - Named universal character escapes
// { dg-do compile { target c++11 } }
// { dg-require-effective-target wchar }
// { dg-options "-pedantic" }

const char32_t *a = U"\N{ETHIOPIC SYLLABLE SEE}";	// { dg-warning "named universal character escapes are only valid in" "" { target c++20_down } }
