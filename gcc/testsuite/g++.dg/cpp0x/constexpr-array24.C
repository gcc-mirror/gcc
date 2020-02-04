// PR c++/90951
// { dg-do compile { target c++11 } }

#define assert(expr) static_assert (expr, #expr)

struct S { const char a[2]; };

constexpr struct S a[1][1][1] = { };

assert ('\0' == *a[0][0][0].a);
