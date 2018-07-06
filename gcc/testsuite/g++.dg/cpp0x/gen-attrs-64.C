// PR c++/85140
// { dg-do compile { target c++11 } }

namespace alignas() N  {}	// { dg-error "expected" }
