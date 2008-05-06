/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Expected errors for char16_t character constants. */
/* { dg-do compile } */
/* { dg-options "-std=c++0x" } */

const static char16_t	c0 = u'';		/* { dg-error "empty character" } */
const static char16_t	c1 = u'ab';		/* { dg-warning "constant too long" } */
const static char16_t	c2 = u'\U00064321';	/* { dg-warning "constant too long" } */

const static char16_t	c3 = 'a';
const static char16_t	c4 = U'a';
const static char16_t	c5 = U'\u2029';
const static char16_t	c6 = U'\U00064321';	/* { dg-warning "implicitly truncated" } */
const static char16_t	c7 = L'a';
const static char16_t	c8 = L'\u2029';
const static char16_t	c9 = L'\U00064321';	/* { dg-warning "implicitly truncated" "" { target { 4byte_wchar_t } } 16 } */
						/* { dg-warning "constant too long" "" { target { ! 4byte_wchar_t } } 16 } */
int main () {}
