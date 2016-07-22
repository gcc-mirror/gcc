// PR c++/66260
// { dg-do assemble { target c++14 } }

template <class>
constexpr bool foo = false;
template <>
constexpr bool foo<int> = true;
template <class T, int N>
constexpr bool foo<T[N]> = foo<T>;

static_assert(foo<int>, "");
static_assert(!foo<double>, "");
static_assert(foo<int[3]>, "");
static_assert(!foo<double[3]>, "");
static_assert(foo<int[2][5][3]>, "");
static_assert(!foo<double[2][5][3]>, "");
