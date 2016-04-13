// PR c++/56582
// { dg-do compile { target c++11 } }

// Reliable ICE
constexpr int n[3] = {};
constexpr int k = n[-1];            // { dg-error "array subscript" }

// Some random byte
constexpr char c = "foo"[-1000];    // { dg-error "array subscript" }
