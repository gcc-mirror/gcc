/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test parsing of u and U prefixes when also used as macros. */
/* { dg-do compile } */
/* { dg-options "-std=gnu++0x" } */

#define u	L
#define U	L

const unsigned short	c2	= u'a';
const unsigned long	c3	= U'a';
const void		*s0	= u"a";
const void		*s1	= U"a";

int main () {}
