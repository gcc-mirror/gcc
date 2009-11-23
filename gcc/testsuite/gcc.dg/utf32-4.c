/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Expected errors for char32_t character constants. */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

typedef __CHAR32_TYPE__ char32_t;

char32_t	c0 = U'';		/* { dg-error "empty character" } */
char32_t	c1 = U'ab';		/* { dg-warning "constant too long" } */
char32_t	c2 = U'\U00064321';

char32_t	c3 = 'a';
char32_t	c4 = u'a';
char32_t	c5 = u'\u2029';
char32_t	c6 = u'\U00064321';	/* { dg-warning "constant too long" } */
char32_t	c7 = L'a';
char32_t	c8 = L'\u2029';
char32_t	c9 = L'\U00064321';     /* { dg-warning "constant too long" "" { target { ! 4byte_wchar_t } } } */

int main () {}
