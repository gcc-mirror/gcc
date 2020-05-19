// { dg-do compile }
// { dg-options "-std=gnu99" { target c } }
// { dg-options "-std=c++0x" { target c++ } }

const void *s0 = R"0123456789abcdefg()0123456789abcdefg" 0;
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s1 = R" () " 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s2 = R"	()	" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s3 = R")())" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s4 = R"@()@" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s5 = R"$()$" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s6 = R"\u0040()\u0040" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }

int main () {}
