// PR c++/83921
// { dg-do compile { target c++14 } }

struct Foo { Foo() = default; };
constexpr void test() { Foo f; }
