/* If not gnu99, the {,u,u8,U,L}R prefix should be parsed as separate
   token. */
/* { dg-do compile } */
/* { dg-options "" } */

const void	*s0	= R"[a]";	/* { dg-error "undeclared" } */
		/* { dg-error "expected ',' or ';'" "" { target *-*-* } 6 } */
const void	*s1	= uR"[a]";	/* { dg-error "undeclared" } */
		/* { dg-error "expected ',' or ';'" "" { target *-*-* } 8 } */
const void	*s2	= UR"[a]";	/* { dg-error "undeclared" } */
		/* { dg-error "expected ',' or ';'" "" { target *-*-* } 10 } */
const void	*s3	= u8R"[a]";	/* { dg-error "undeclared" } */
		/* { dg-error "expected ',' or ';'" "" { target *-*-* } 12 } */
const void	*s4	= LR"[a]";	/* { dg-error "undeclared" } */
		/* { dg-error "expected ',' or ';'" "" { target *-*-* } 14 } */

const int	i0	= R'a';		/* { dg-error "expected ',' or ';'" } */
const int	i1	= uR'a';	/* { dg-error "expected ',' or ';'" } */
const int	i2	= UR'a';	/* { dg-error "expected ',' or ';'" } */
const int	i3	= u8R'a';	/* { dg-error "expected ',' or ';'" } */
const int	i4	= LR'a';	/* { dg-error "expected ',' or ';'" } */

#define R	"a"
#define uR	"b"
#define UR	"c"
#define u8R	"d"
#define LR	"e"

const void	*s5	= R"[a]";
const void	*s6	= uR"[a]";
const void	*s7	= UR"[a]";
const void	*s8	= u8R"[a]";
const void	*s9	= LR"[a]";

#undef R
#undef uR
#undef UR
#undef u8R
#undef LR

#define R	1 +
#define uR	2 +
#define UR	3 +
#define u8R	4 +
#define LR	5 +

const int	i5	= R'a';
const int	i6	= uR'a';
const int	i7	= UR'a';
const int	i8	= u8R'a';
const int	i9	= LR'a';

int main () {}
