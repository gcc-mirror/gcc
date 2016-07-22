// PR c++/50258
// { dg-do compile { target c++11 } }
// { dg-options "-fpermissive" }

struct Foo {
  static const double d = 3.14; // { dg-warning "23:'constexpr' needed" }
};
const double Foo::d;
