// PR c++/50258
// { dg-options "-std=c++0x -fpermissive" }

struct Foo {
  static const double d = 3.14; // { dg-warning "constexpr" }
};
const double Foo::d;            // { dg-warning "constexpr" }
