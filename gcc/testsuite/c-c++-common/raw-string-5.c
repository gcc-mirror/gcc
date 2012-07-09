// { dg-do compile }
// { dg-options "-std=gnu99" { target c } }
// { dg-options "-std=c++0x" { target c++ } }

const void *s0 = R"0123456789abcdefg()0123456789abcdefg";
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } 5 }
	// { dg-error "stray" "stray" { target *-*-* } 5 }
const void *s1 = R" () ";
	// { dg-error "invalid character" "invalid" { target *-*-* } 8 }
	// { dg-error "stray" "stray" { target *-*-* } 8 }
const void *s2 = R"	()	";
	// { dg-error "invalid character" invalid"" { target *-*-* } 11 }
	// { dg-error "stray" "stray" { target *-*-* } 11 }
const void *s3 = R")())";
	// { dg-error "invalid character" invalid"" { target *-*-* } 14 }
	// { dg-error "stray" "stray" { target *-*-* } 14 }
const void *s4 = R"@()@";
	// { dg-error "invalid character" invalid"" { target *-*-* } 17 }
	// { dg-error "stray" "stray" { target *-*-* } 17 }
const void *s5 = R"$()$";
	// { dg-error "invalid character" invalid"" { target *-*-* } 20 }
	// { dg-error "stray" "stray" { target *-*-* } 20 }
const void *s6 = R"\u0040()\u0040";
	// { dg-error "invalid character" invalid"" { target *-*-* } 23 }
	// { dg-error "stray" "stray" { target *-*-* } 23 }

int main () {}
