// PR c++/87175
// { dg-do compile }
// { dg-options "" }

struct __attribute__)) foo { };	// { dg-error "expected" }
struct __attribute__()) bar { };// { dg-error "expected" }
