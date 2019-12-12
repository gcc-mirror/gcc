// { dg-do compile { target c++11 } }

int i = 42;
constexpr int *p = &i;
constexpr int const *const *q = &p;
constexpr int const *r = *q;
