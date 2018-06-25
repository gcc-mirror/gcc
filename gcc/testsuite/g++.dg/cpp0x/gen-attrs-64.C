// PR c++/85140
// { dg-do compile { target c++11 } }

namespace N alignas() {}	// { dg-error "expected" }
