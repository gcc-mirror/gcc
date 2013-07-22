// PR preprocessor/57620
// { dg-do compile }
// { dg-options "-std=gnu99 -trigraphs" { target c } }
// { dg-options "-std=c++11" { target c++ } }

const void *s0 = R"abc\
def()abcdef";
	// { dg-error "invalid character" "invalid" { target *-*-* } 6 }
	// { dg-error "stray" "stray" { target *-*-* } 6 }
const void *s1 = R"??/
()??/";
	// { dg-error "invalid new-line" "invalid" { target *-*-* } 10 }
	// { dg-error "stray" "stray" { target *-*-* } 10 }
	// { dg-warning "missing terminating" "missing" { target *-*-* } 10 }
	// { dg-error "missing terminating" "missing" { target *-*-* } 10 }
const void *s2 = R"abcdefghijklmn??/(a)abcdefghijklmn???";
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } 16 }
	// { dg-error "stray" "stray" { target *-*-* } 16 }
	// { dg-error "expected" "expected" { target *-*-* } 16 }
const void *s3 = R"abcdefghijklmno??/(a)abcdefghijklmno???";
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } 20 }
	// { dg-error "stray" "stray" { target *-*-* } 20 }
const void *s4 = R"abcdefghijklmnop??=(a)abcdefghijklmnop??=";
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } 23 }
	// { dg-error "stray" "stray" { target *-*-* } 23 }
const void *s5 = R"abc\
()abcdef";
	// { dg-error "invalid character" "invalid" { target *-*-* } 26 }
	// { dg-error "stray" "stray" { target *-*-* } 26 }
const void *s6 = R"\
()";
	// { dg-error "invalid character" "invalid" { target *-*-* } 30 }
	// { dg-error "stray" "stray" { target *-*-* } 30 }
const void *s7 = R"\
a()a";
	// { dg-error "invalid character" "invalid" { target *-*-* } 34 }
	// { dg-error "stray" "stray" { target *-*-* } 34 }

int main () {}
