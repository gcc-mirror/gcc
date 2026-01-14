// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::tuple_{size,element}.

#include <array>
#include <complex>
#include <meta>
#include <ranges>
#include <tuple>

using namespace std::meta;

struct E {};

using Tup0 = std::tuple<>;
static_assert (tuple_size (^^Tup0) == 0);

using Tup1 = std::tuple<int>;
static_assert (tuple_size (^^Tup1) == 1);
static_assert (tuple_element (0, ^^Tup1) == ^^int);

using Tup2 = std::tuple<int, bool>;
static_assert (tuple_size (^^Tup2) == 2);
static_assert (tuple_element (0, ^^Tup2) == ^^int);
static_assert (tuple_element (1, ^^Tup2) == ^^bool);

using Tup3 = std::tuple<int, bool, char>;
static_assert (tuple_size (^^Tup3) == 3);
static_assert (tuple_element (0, ^^Tup3) == ^^int);
static_assert (tuple_element (1, ^^Tup3) == ^^bool);
static_assert (tuple_element (2, ^^Tup3) == ^^char);

using Tup10 = std::tuple<int, int, int, int, int, int, int, int, int, int>;
static_assert (tuple_size (^^Tup10) == 10);
static_assert (tuple_element (0, ^^Tup10) == ^^int);
static_assert (tuple_element (9, ^^Tup10) == ^^int);

using mytype1 = float;
using mytype2 = mytype1 *;
static_assert (tuple_size (^^std::tuple<mytype1, mytype2>) == 2);
static_assert (tuple_element (0, ^^std::tuple<mytype1>) == ^^float);
static_assert (tuple_element (1, ^^std::tuple<mytype1, mytype2>) == ^^float *);

static_assert (tuple_size (^^std::tuple<>) == 0);
static_assert (tuple_size (^^std::tuple<int>) == 1);
static_assert (tuple_size (^^std::tuple<void>) == 1);
static_assert (tuple_size (^^std::tuple<std::tuple<void>>) == 1);
static_assert (tuple_size (^^const std::tuple<>) == 0);
static_assert (tuple_size (^^const std::tuple<int>) == 1);
static_assert (tuple_size (^^const std::tuple<void>) == 1);
static_assert (tuple_size (^^const std::tuple<std::tuple<void>>) == 1);

using Arr5 = std::array<int, 5>;
static_assert (tuple_size (^^Arr5) == 5);
static_assert (tuple_size (^^const Arr5) == 5);
static_assert (tuple_element (0, ^^Arr5) == ^^int);
static_assert (tuple_element (1, ^^Arr5) == ^^int);
static_assert (tuple_element (2, ^^Arr5) == ^^int);
static_assert (tuple_element (3, ^^Arr5) == ^^int);
static_assert (tuple_element (4, ^^Arr5) == ^^int);
using Arr0 = std::array<int, 0>;
static_assert (tuple_size (^^Arr0) == 0);

using Pair = std::pair<E, int>;
// Always 2.
static_assert (tuple_size (^^Pair) == 2);
static_assert (tuple_element (0, ^^Pair) == ^^E);
static_assert (tuple_element (1, ^^Pair) == ^^int);

using C = std::complex<double>;
static_assert (tuple_size (^^C) == 2);
static_assert (tuple_element (0, ^^C) == ^^double);
static_assert (tuple_element (1, ^^C) == ^^double);

using S1 = std::ranges::subrange<int *>;
using S2 = std::ranges::subrange<long *, void *>;
static_assert (tuple_size (^^S1) == 2);
static_assert (tuple_size (^^S2) == 2);
static_assert (tuple_element (0, ^^S1) == ^^int *);
static_assert (tuple_element (1, ^^S1) == ^^int *);
static_assert (tuple_element (0, ^^const S1) == ^^int *);
static_assert (tuple_element (1, ^^const S1) == ^^int *);
static_assert (tuple_element (0, ^^S2) == ^^long *);
static_assert (tuple_element (1, ^^S2) == ^^void *);
static_assert (tuple_element (0, ^^const S2) == ^^long *);
static_assert (tuple_element (1, ^^const S2) == ^^void *);
