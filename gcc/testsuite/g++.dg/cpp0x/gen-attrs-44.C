// PR c++/52671
// { dg-do compile { target c++11 } }
[[gnu::deprecated]] enum E { E0 };	// { dg-warning "ignored in declaration" }
