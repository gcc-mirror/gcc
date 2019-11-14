// Test that we suggest adding #include <compare>.
// { dg-do compile { target c++2a } }

auto x = 1<=>2;			// { dg-error "" }
// { dg-message "<compare>" "" { target *-*-* } .-1 }
