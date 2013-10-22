// PR c++/47207
// { dg-options -std=c++11 }

constexpr int X (X);		// { dg-error "not usable" }
// { dg-message "own initializer" "" { target *-*-* } 4 }
