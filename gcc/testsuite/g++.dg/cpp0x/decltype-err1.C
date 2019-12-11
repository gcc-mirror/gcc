// PR c++/92105
// { dg-do compile { target c++11 } }

// Test that we get exactly one "expected" error.

decltype(decltype) x = 42;	// { dg-bogus "expected.*expected" }
// { dg-error "expected" "" { target *-*-* } .-1 }
