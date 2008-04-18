/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Ensure that type specifiers are not allowed for char16_t/char32_t. */
/* { dg-do compile } */
/* { dg-options "-std=c++0x" } */

signed char16_t		c0;		/* { dg-error "signed" } */
signed char32_t		c1;		/* { dg-error "signed" } */
unsigned char16_t	c2;		/* { dg-error "unsigned" } */
unsigned char32_t	c3;		/* { dg-error "unsigned" } */

short char16_t		c4;		/* { dg-error "short" } */
long char16_t		c5;		/* { dg-error "long" } */
short char32_t		c6;		/* { dg-error "short" } */
long char32_t		c7;		/* { dg-error "long" } */

signed short char16_t	c8;		/* { dg-error "signed" } */
signed short char32_t	c9;		/* { dg-error "signed" } */
signed long char16_t	ca;		/* { dg-error "signed" } */
signed long char32_t	cb;		/* { dg-error "signed" } */
unsigned short char16_t	cc;		/* { dg-error "unsigned" } */
unsigned short char32_t	cd;		/* { dg-error "unsigned" } */
unsigned long char16_t	ce;		/* { dg-error "unsigned" } */
unsigned long char32_t	cf;		/* { dg-error "unsigned" } */

int main () {}
