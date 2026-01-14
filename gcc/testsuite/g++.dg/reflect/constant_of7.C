// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::constant_of.

#include <meta>

using namespace std::meta;

constexpr int arr[] = { 1, 2, 3, 4 };
constexpr auto r = constant_of (^^arr);
static_assert (!is_value (r) && !is_object (r));
static_assert (*[:r:] == 1);
static_assert ([:r:][0] == 1);
static_assert ([:r:][1] == 2);
static_assert ([:r:][2] == 3);
static_assert ([:r:][3] == 4);
static_assert (type_of (r) == ^^const int [4]);
constexpr auto q = reflect_constant ([: ^^arr :]);
static_assert (type_of (q) == ^^const int *);
static_assert ([:q:][0] == 1);
static_assert ([:q:][1] == 2);
static_assert ([:q:][2] == 3);
static_assert ([:q:][3] == 4);

using U = int;
constexpr U arr2[] = { 5, 6, 7, 8 };
constexpr auto r2 = constant_of (^^arr2);
static_assert (!is_value (r2) && !is_object (r2));
static_assert ([:r2:][0] == 5);
static_assert ([:r2:][1] == 6);
static_assert ([:r2:][2] == 7);
static_assert ([:r2:][3] == 8);
// ??? Probably should be const int *.
static_assert (type_of (r2) == ^^const U [4]);
constexpr auto q2 = reflect_constant ([: ^^arr2 :]);
static_assert (type_of (q2) == ^^const U *);
static_assert ([:q2:][0] == 5);
static_assert ([:q2:][1] == 6);
static_assert ([:q2:][2] == 7);
static_assert ([:q2:][3] == 8);

struct S { int m; };
constexpr S arr3[] = { S{1}, S{2}, S{3} };
constexpr auto r3 = constant_of (^^arr3);
static_assert (!is_value (r3) && !is_object (r3));
static_assert ([:r3:][0].m == 1);
static_assert ([:r3:][1].m == 2);
static_assert ([:r3:][2].m == 3);
static_assert (type_of (r3) == ^^const S [3]);
constexpr auto q3 = reflect_constant ([: ^^arr3 :]);
static_assert (type_of (q3) == ^^const S *);
static_assert ([:q3:][0].m == 1);
static_assert ([:q3:][1].m == 2);
static_assert ([:q3:][2].m == 3);
