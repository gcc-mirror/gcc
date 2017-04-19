/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* If not gnu99, the u and U prefixes should be parsed as separate tokens. */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

const unsigned short	c0	= u'a';		/* { dg-error "undeclared" "undeclared" } */
		/* { dg-error "expected ',' or ';'" "expected" { target *-*-* } .-1 } */
const unsigned long	c1	= U'a';		/* { dg-error "undeclared" "undeclared" } */
		/* { dg-error "expected ',' or ';'" "expected" { target *-*-* } .-1 } */

#define u	1 +
#define U	2 +

const unsigned short	c2	= u'a';
const unsigned long	c3	= U'a';

#undef u
#undef U
#define u	"a"
#define U	"b"

const void		*s0	= u"a";
const void		*s1	= U"a";

int main () {}
