// PR c++/113925
// { dg-do compile { target c++20 } }

template<bool B>
struct b{};
static_assert(requires { b<([]()consteval{ return true; }())>{}; });
