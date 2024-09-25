// PR preprocessor/57620
// { dg-do compile { target { c || c++11 } } }
// { dg-options "-std=gnu99 -trigraphs" { target c } }

const void *s0 = R"abc\
def()abcdef" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-2 }
	// { dg-error "stray" "stray" { target *-*-* } .-3 }
const void *s1 = R"??/
()??/" 0;
	// { dg-error "invalid new-line" "invalid" { target *-*-* } .-2 }
	// { dg-error "stray" "stray" { target *-*-* } .-3 }
const void *s2 = R"abcdefghijklmn??/(a)abcdefghijklmn???" 0;
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s3 = R"abcdefghijklmno??/(a)abcdefghijklmno???" 0;
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s4 = R"abcdefghijklmnop??=(a)abcdefghijklmnop??=" 0;
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } .-1 }
	// { dg-error "stray" "stray" { target *-*-* } .-2 }
const void *s5 = R"abc\
()abcdef" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-2 }
	// { dg-error "stray" "stray" { target *-*-* } .-3 }
const void *s6 = R"\
()" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-2 }
	// { dg-error "stray" "stray" { target *-*-* } .-3 }
const void *s7 = R"\
a()a" 0;
	// { dg-error "invalid character" "invalid" { target *-*-* } .-2 }
	// { dg-error "stray" "stray" { target *-*-* } .-3 }

int main () {}
