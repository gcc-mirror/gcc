// R is not applicable for character literals.
// { dg-do compile }
// { dg-options "-std=gnu99" { target c } }
// { dg-options "-std=c++0x" { target c++ } }

const int	i0	= R'a';	// { dg-error "was not declared|undeclared" "undeclared" }
		// { dg-error "expected ',' or ';'" "expected" { target c } .-1 }
const int	i1	= uR'a';	// { dg-error "was not declared|undeclared" "undeclared" }
		// { dg-error "expected ',' or ';'" "expected" { target c } .-1 }
const int	i2	= UR'a';	// { dg-error "was not declared|undeclared" "undeclared" }
		// { dg-error "expected ',' or ';'" "expected" { target c } .-1 }
const int	i3	= u8R'a';	// { dg-error "was not declared|undeclared" "undeclared" }
		// { dg-error "expected ',' or ';'" "expected" { target c } .-1 }
const int	i4	= LR'a';	// { dg-error "was not declared|undeclared" "undeclared" }
		// { dg-error "expected ',' or ';'" "expected" { target c } .-1 }

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
