// DR 1312 - Simulated reinterpret_cast in constant expressions.
// PR c++/99176
// { dg-do compile { target c++11 } }

static int i;
constexpr void *vp0 = nullptr;
constexpr void *vpi = &i;
constexpr int *p1 = (int *) vp0; // { dg-error "cast from .void\\*. is not allowed" }
constexpr int *p2 = (int *) vpi; // { dg-error "cast from .void\\*. is not allowed" }
constexpr int *p3 = static_cast<int *>(vp0); // { dg-error "cast from .void\\*. is not allowed" }
constexpr int *p4 = static_cast<int *>(vpi); // { dg-error "cast from .void\\*. is not allowed" }
constexpr void *p5 = vp0;
constexpr void *p6 = vpi;

constexpr int *pi = &i;
constexpr bool b = ((int *)(void *) pi == pi); // { dg-error "cast from .void\\*. is not allowed" }
