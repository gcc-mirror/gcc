// { dg-do compile { target c++17 } }

inline int a = 0;
inline int b = 0;
static_assert (&a == &a);
static_assert (&a != &b);
constexpr bool c = &a == &a;
constexpr bool d = &a != &b;
