// PR c++/67376
// { dg-do compile { target c++11 } }

struct A { int e[2]; };
constexpr A a { { 0, 1 } };
static_assert (a.e + 1 != a.e, "");
static_assert (a.e != a.e + 1, "");
static_assert (a.e + 2 != a.e, "");
static_assert (a.e != a.e + 2, "");
static_assert (a.e + 1 > a.e, "");
static_assert (a.e < a.e + 1, "");
static_assert (a.e + 2 > a.e, "");
static_assert (a.e < a.e + 2, "");
static_assert (a.e + 1 >= a.e, "");
static_assert (a.e <= a.e + 1, "");
static_assert (a.e + 2 >= a.e, "");
static_assert (a.e <= a.e + 2, "");
