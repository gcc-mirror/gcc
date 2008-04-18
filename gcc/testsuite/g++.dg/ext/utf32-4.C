/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Expected errors for char32_t character constants. */
/* { dg-do compile } */
/* { dg-options "-std=c++0x" } */

const static char32_t	c0 = U'';		/* { dg-error "empty character" } */
const static char32_t	c1 = U'ab';		/* { dg-warning "constant too long" } */
const static char32_t	c2 = U'\U00064321';

const static char32_t	c3 = 'a';
const static char32_t	c4 = u'a';
const static char32_t	c5 = u'\u2029';
const static char32_t	c6 = u'\U00064321';	/* { dg-warning "constant too long" } */
const static char32_t	c7 = L'a';
const static char32_t	c8 = L'\u2029';
const static char32_t	c9 = L'\U00064321';

int main () {}
