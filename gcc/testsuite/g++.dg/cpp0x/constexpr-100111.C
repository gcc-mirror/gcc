// PR c++/100111
// { dg-do compile { target c++11 } }
// { dg-options "-fno-elide-constructors" }

struct A {};
struct B : A { int b; constexpr B (A x) : A(x), b() {} };
struct C { B c; constexpr C () : c({}) {} } d;
