// { dg-do compile { target c++11 } }

typedef long vec __attribute__((vector_size(2*sizeof(long))));
constexpr vec v = { 33, 42 };
constexpr auto l0 = v[0];
constexpr auto l1 = v[1];
static_assert(l0==33,"Fail");
static_assert(l1==42,"Fail");
