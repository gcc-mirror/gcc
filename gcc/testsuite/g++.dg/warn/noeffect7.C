// PR c++/8057
// Don't give a "statement has no effect" warning when declaring a
// template, only when instantiating it.
// { dg-do compile }
// { dg-options "-Wunused" }
struct Y { static int i; };
template <typename T> class X { X() { Y::i; }; };
class Z { Z() { Y::i; }; }; // { dg-warning "no effect" }
