// { dg-do compile { target { c || c++11 } } }
// { dg-options "-std=gnu99" { target c } }

const void *s0 = R"ouch()ouCh"; 	// { dg-error "unterminated raw string" "unterminated" }
// { dg-error "at end of input" "end" { target *-*-* } .-1 }
