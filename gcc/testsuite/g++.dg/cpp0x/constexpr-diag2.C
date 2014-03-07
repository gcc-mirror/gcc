// PR c++/47207
// { dg-do compile { target c++11 } }

constexpr int X (X);		// { dg-error "not usable" }
// { dg-message "own initializer" "" { target *-*-* } 4 }
