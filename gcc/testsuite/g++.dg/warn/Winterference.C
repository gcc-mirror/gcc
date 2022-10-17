// Test that we warn about use of std::hardware_destructive_interference_size
// in a header.
// { dg-do compile { target c++17 } }

// { dg-warning Winterference-size "" { target *-*-* } 0 }
#include "Winterference.H"
