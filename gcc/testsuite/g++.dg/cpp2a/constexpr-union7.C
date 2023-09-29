// { dg-do compile { target c++14 } }
// { dg-options "" }

union U { int i; float f; };
constexpr auto g (U u) { return (u.i = 42); } // { dg-error "active member" "" { target c++17_down } }
static_assert (g({.f = 3.14}) == 42); // { dg-error "non-constant" "" { target c++17_down } }
