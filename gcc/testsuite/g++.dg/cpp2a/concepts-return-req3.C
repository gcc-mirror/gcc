// PR c++/100946
// { dg-do compile { target c++20 } }

template<class T> concept C = __is_same(T, int);
static_assert(requires { { 0 } -> C; });
static_assert(requires { { true } -> C; }); // { dg-error "failed" }
