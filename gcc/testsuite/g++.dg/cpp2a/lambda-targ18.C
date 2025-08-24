// PR c++/107953
// { dg-do compile { target c++20 } }

template<auto F>
struct Foo {};

Foo<[](){ return 1 >= 0; }> foo1{};

Foo<[](){ return (1 > 0); }> foo2{};

Foo<[](){ return 1 > 0; }> foo3{};

Foo<[g = 1 > 0]{ return g; }> foo4{};
