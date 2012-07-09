// { dg-do compile }
// { dg-options "-std=gnu99" { target c } }
// { dg-options "-std=c++0x" { target c++ } }

const void *s0 = R"ouch()ouCh";	// { dg-error "at end of input" "end" }
	// { dg-error "unterminated raw string" "unterminated" { target *-*-* } 5 }
