// { dg-do compile { target c++17 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

inline int a = 0;
inline int b = 0;
static_assert (&a == &a);
static_assert (&a != &b);
constexpr bool c = &a == &a;
constexpr bool d = &a != &b;
