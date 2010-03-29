// { dg-do compile }
// { dg-options "-std=c++0x" }

const void *s0 = R"0123456789abcdefg()0123456789abcdefg";
	// { dg-error "raw string delimiter longer" "" { target *-*-* } 4 }
	// { dg-error "stray" "" { target *-*-* } 4 }
const void *s1 = R" () ";
	// { dg-error "invalid character" "" { target *-*-* } 7 }
	// { dg-error "stray" "" { target *-*-* } 7 }
const void *s2 = R"	()	";
	// { dg-error "invalid character" "" { target *-*-* } 10 }
	// { dg-error "stray" "" { target *-*-* } 10 }
const void *s3 = R")())";
	// { dg-error "invalid character" "" { target *-*-* } 13 }
	// { dg-error "stray" "" { target *-*-* } 13 }
const void *s4 = R"@()@";
	// { dg-error "invalid character" "" { target *-*-* } 16 }
	// { dg-error "stray" "" { target *-*-* } 16 }
const void *s5 = R"$()$";
	// { dg-error "invalid character" "" { target *-*-* } 19 }
	// { dg-error "stray" "" { target *-*-* } 19 }
const void *s6 = R"\u0010()\u0010";
	// { dg-error "invalid character" "" { target *-*-* } 22 }
	// { dg-error "stray" "" { target *-*-* } 22 }

int main () {}
