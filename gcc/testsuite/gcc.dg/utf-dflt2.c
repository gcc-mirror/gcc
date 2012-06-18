/* If not gnu99, the u8 prefix should be parsed as separate tokens. */
/* { dg-do compile } */
/* { dg-options "" } */

const void	*s0 = u8"a";		/* { dg-error "undeclared" "undeclared" } */
		/* { dg-error "expected ',' or ';'" "expected" { target *-*-* } 5 } */

#define u8	"a"

const void	*s1 = u8"a";

int main () {}
