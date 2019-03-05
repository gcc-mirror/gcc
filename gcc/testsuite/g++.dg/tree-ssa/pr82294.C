// PR c++/82294
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-gimple" }

// Verify we don't "optimize" the ctor as copying a 1KB .rodata
// object into the variable.  It is better to initialize it through
// a loop.
// { dg-final { scan-tree-dump-not "this->arr = " "gimple" } }

struct S { int x; explicit constexpr S (); };
constexpr S::S () : x{7} {}
struct T { S arr[256]; explicit T (); };
T::T () {}
