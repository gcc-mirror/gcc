// PR c++/114426
// { dg-do compile { target c++11 } }

struct A { virtual ~A (); };
struct B : virtual A { constexpr ~B () {} };
// { dg-error "'struct B' has virtual base classes" "" { target c++20 } .-1 }
// { dg-error "'constexpr' destructors only available with" "" { target c++17_down } .-2 }
