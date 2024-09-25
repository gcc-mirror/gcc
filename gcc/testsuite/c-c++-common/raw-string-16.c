// PR preprocessor/57620
// { dg-do compile { target { c || c++11 } } }
// { dg-options "-std=gnu99 -Wtrigraphs" { target c } }
// { dg-options "-Wtrigraphs" { target c++ } }

const void *s0 = R"abc\
def()abcdef" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } 6 }
	// { dg-error "stray" "stray" { target *-*-* } 6 }
const void *s1 = R"abcdefghijklmn??/(a)abcdefghijklmn???" 0;
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }

const void *s2 = R"abcdefghijklmno??/(a)abcdefghijklmno???" 0;
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s3 = R"abcdefghijklmnop??=(a)abcdefghijklmnop??=?" 0;
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s4 = R"abc\
()abcdef" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } 20 }
	// { dg-error "stray" "stray" { target *-*-* } 20 }
const void *s5 = R"\
()" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } 24 }
	// { dg-error "stray" "stray" { target *-*-* } 24 }
const void *s6 = R"\
a()a" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } 28 }
	// { dg-error "stray" "stray" { target *-*-* } 28 }

int main () {}
