// PR c++/46466
// { dg-do compile { target c++11 } }
// { dg-options "-fno-elide-constructors" }

struct S { bool b; };
constexpr S f() { return S{true}; }
static_assert(f().b,  "");
