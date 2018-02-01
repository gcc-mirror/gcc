// PR c++/83921
// { dg-do compile { target c++14 } }

struct Foo { int m; };
constexpr void test() { Foo f; }  // { dg-error "uninitialized" }
