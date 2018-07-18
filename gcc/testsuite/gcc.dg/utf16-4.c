/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Expected errors for char16_t character constants. */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

typedef __CHAR16_TYPE__ char16_t;

char16_t	c0 = u'';		/* { dg-error "empty character" } */
char16_t	c1 = u'ab';		/* { dg-warning "constant too long" } */
char16_t	c2 = u'\U00064321';	/* { dg-warning "constant too long" } */

char16_t	c3 = 'a';
char16_t	c4 = U'a';
char16_t	c5 = U'\u2029';
char16_t	c6 = U'\U00064321';	/* { dg-warning "conversion from .(long )?unsigned int. to 'char16_t' {aka '(short )?unsigned int'} changes value from .410401. to .17185." } */
char16_t	c7 = L'a';
char16_t	c8 = L'\u2029';
char16_t 	c9 = L'\U00064321';	/* { dg-warning "conversion" "" { target { 4byte_wchar_t } } } */
					/* { dg-warning "constant too long" "" { target { ! 4byte_wchar_t } } .-1 } */

int main () {}
