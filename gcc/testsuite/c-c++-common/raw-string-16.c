// PR preprocessor/57620
// { dg-do compile }
// { dg-options "-std=gnu99 -Wtrigraphs" { target c } }
// { dg-options "-std=gnu++11 -Wtrigraphs" { target c++ } }

const void *s0 = R"abc\
def()abcdef";
	// { dg-error "invalid character" "invalid" { target *-*-* } 6 }
	// { dg-error "stray" "stray" { target *-*-* } 6 }
const void *s1 = R"abcdefghijklmn??/(a)abcdefghijklmn???";
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } 10 }
	// { dg-error "stray" "stray" { target *-*-* } 10 }

const void *s2 = R"abcdefghijklmno??/(a)abcdefghijklmno???";
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } 14 }
	// { dg-error "stray" "stray" { target *-*-* } 14 }
const void *s3 = R"abcdefghijklmnop??=(a)abcdefghijklmnop??=?"; // { dg-warning "trigraph ..= ignored" }
	// { dg-error "raw string delimiter longer" "longer" { target *-*-* } 17 }
	// { dg-error "stray" "stray" { target *-*-* } 17 }
const void *s4 = R"abc\
()abcdef";
	// { dg-error "invalid character" "invalid" { target *-*-* } 20 }
	// { dg-error "stray" "stray" { target *-*-* } 20 }
const void *s5 = R"\
()";
	// { dg-error "invalid character" "invalid" { target *-*-* } 24 }
	// { dg-error "stray" "stray" { target *-*-* } 24 }
const void *s6 = R"\
a()a";
	// { dg-error "invalid character" "invalid" { target *-*-* } 28 }
	// { dg-error "stray" "stray" { target *-*-* } 28 }

int main () {}
