/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Expected errors for char16_t character constants. */
/* { dg-do compile { target c++11 } } */


const static char16_t	c0 = u'';		/* { dg-error "empty character" } */
const static char16_t	c1 = u'ab';		/* { dg-error "multi-character literal cannot have an encoding prefix" } */
const static char16_t	c2 = u'\U00064321';	/* { dg-error "character not encodable in a single code unit" } */

const static char16_t	c3 = 'a';
const static char16_t	c4 = U'a';
const static char16_t	c5 = U'\u2029';
const static char16_t	c6 = U'\U00064321';	/* { dg-warning "conversion from .char32_t. to .char16_t. changes value from .410401. to .17185." } */
const static char16_t	c7 = L'a';
const static char16_t	c8 = L'\u2029';
const static char16_t	c9 = L'\U00064321';	/* { dg-warning "conversion from .wchar_t. to .char16_t. changes value from .410401. to .17185." "" { target { 4byte_wchar_t } } } */
						/* { dg-warning "character not encodable in a single code unit" "" { target { { ! 4byte_wchar_t } && c++20_down } } .-1 } */
						/* { dg-error "character not encodable in a single code unit" "" { target { { ! 4byte_wchar_t } && c++23 } } .-2 } */
int main () {}
