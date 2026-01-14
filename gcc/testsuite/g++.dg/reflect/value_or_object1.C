// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_value and is_object.

#include <meta>

using namespace std::meta;

constexpr int i = 42;

template<int N>
constexpr int v = N;

static_assert (!is_object (^^i));
static_assert (!is_value (^^i));
static_assert (is_variable (^^i));
static_assert (!is_object (reflect_constant (^^i)));
static_assert (is_value (reflect_constant (^^i)));
static_assert (!is_variable (reflect_constant (^^i)));
static_assert (!is_object (substitute (^^v, { reflect_constant (15) })));
static_assert (!is_object (^^v<15>));
static_assert (!is_value (^^v<15>));
static_assert (is_variable (^^v<15>));
static_assert (is_value (reflect_constant (^^v<15>)));
static_assert (!is_object (reflect_constant (^^v<15>)));
static_assert (!is_variable (reflect_constant (^^v<15>)));

constexpr std::pair<std::pair<int, bool>, int> p = {{1, true}, 2};
constexpr info rfirst = reflect_object (p.first);
static_assert (is_object (rfirst) && !is_value (rfirst));
static_assert (type_of (rfirst) == ^^const std::pair<int, bool>);
static_assert (rfirst != reflect_constant (std::make_pair (1, true)));

constexpr int k = 3;
constexpr const int *pk = &k;
constexpr const int &ref = k;
constexpr auto rp = ^^pk;
constexpr auto rref = ^^ref;
static_assert (is_variable (rp));
static_assert (!is_object (rp));
static_assert (!is_value (rp));
static_assert (!is_variable (reflect_constant (rp)));
static_assert (!is_object (reflect_constant (rp)));
static_assert (is_value (reflect_constant (rp)));
static_assert (is_variable (rref));
static_assert (!is_object (rref));
static_assert (!is_value (rref));
static_assert (!is_variable (reflect_constant (rref)));
static_assert (!is_object (reflect_constant (rref)));
static_assert (is_value (reflect_constant (rref)));
