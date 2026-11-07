// PR c++/128532
// The pre-C++20 designated initializer extension was more flexible than what
// was standardized.

// { dg-additional-options -Wno-c++20-extensions }

struct A { int a, b; };
struct S { A x; int c; };
S s = { 1, .b = 2 };	       // { dg-error "designated" "" { target c++20 } }
