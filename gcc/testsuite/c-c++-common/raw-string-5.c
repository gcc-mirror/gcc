// { dg-do compile { target { c || c++11 } } }
// { dg-options "-std=gnu99" { target c } }

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
	// { dg-error "invalid character" "invalid" { target { c || c++23_down } } .-1 }
	// { dg-error "stray" "stray" { target { c || c++23_down } } .-2 }
	// { dg-error "before numeric constant" "numeric" { target c++26 } .-3 }
const void *s5 = R"$()$" 0;
	// { dg-error "invalid character" "invalid" { target { c || c++23_down } } .-1 }
	// { dg-error "stray" "stray" { target { c || c++23_down } } .-2 }
	// { dg-error "before numeric constant" "numeric" { target c++26 } .-3 }
const void *s6 = R"`()`" 0;
	// { dg-error "invalid character" "invalid" { target { c || c++23_down } } .-1 }
	// { dg-error "stray" "stray" { target { c || c++23_down } } .-2 }
	// { dg-error "before numeric constant" "numeric" { target c++26 } .-3 }
const void *s7 = R"\u0040()\u0040" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }

int main () {}
