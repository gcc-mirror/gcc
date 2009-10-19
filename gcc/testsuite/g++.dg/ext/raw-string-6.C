// { dg-do compile }
// { dg-options "-std=c++0x" }

const void *s0 = R"ouch[]ouCh";	// { dg-error "at end of input" }
	// { dg-error "unterminated raw string" "" { target *-*-* } 4 }
