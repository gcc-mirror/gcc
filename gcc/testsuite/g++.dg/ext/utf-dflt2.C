// In C++0x, the u8 prefix should be parsed as separate tokens.
// { dg-do compile }
// { dg-options "-std=c++98" }

const void	*s0 = u8"a";		// { dg-error "was not declared" }

#define u8	"a"

const void	*s1 = u8"a";

int main () {}
