// C++ 26 P3394R4 - Annotations for Reflection
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::annotations_of.

#include <meta>

using namespace std::meta;

template<class, class> inline constexpr bool same_type_v = false;
template<class T> inline constexpr bool same_type_v<T, T> = true;

[[=42, =42]] int x;
static_assert (annotations_of (^^x).size () == 2);
static_assert (same_type_v<decltype (annotations_of (^^x)), std::vector<info>>);
static_assert (type_of (annotations_of (^^x)[0]) == ^^int);
static_assert (type_of (annotations_of (^^x)[1]) == ^^int);

[[=42]] int foo ();
[[=24L]] int foo ();
static_assert (annotations_of (^^foo).size () == 2);
static_assert (type_of (annotations_of (^^foo)[0]) == ^^int);
static_assert (type_of (annotations_of (^^foo)[1]) == ^^long);

[[=1]] void bar ();
[[=2U, =3UL]] void baz ();
void baz [[=4ULL]] ();

static_assert (annotations_of (^^bar).size () == 1);
static_assert (type_of (annotations_of (^^bar)[0]) == ^^int);
static_assert (annotations_of (^^baz).size () == 3);
static_assert (type_of (annotations_of (^^baz)[0]) == ^^unsigned);
static_assert (type_of (annotations_of (^^baz)[1]) == ^^unsigned long);
static_assert (type_of (annotations_of (^^baz)[2]) == ^^unsigned long long);

struct [[=42]] C {};
constexpr std::meta::info a0 = annotations_of (^^C)[0];
static_assert (is_annotation (a0));
static_assert (type_of (a0) == ^^int);

template <class T>
struct [[=42]] D {};

//constexpr std::meta::info a1 = annotations_of (^^D<int>)[0];
//constexpr std::meta::info a2 = annotations_of (^^D<char>)[0];
//static_assert (is_annotation (a1) && is_annotation (a2));

[[=1, =2L, =3.0, =4U, =5U, =6L, =7U]] int y;
static_assert (annotations_of (^^y).size () == 7);
static_assert (type_of (annotations_of (^^y)[0]) == ^^int);
static_assert (type_of (annotations_of (^^y)[1]) == ^^long);
static_assert (type_of (annotations_of (^^y)[2]) == ^^double);
static_assert (type_of (annotations_of (^^y)[3]) == ^^unsigned);
static_assert (type_of (annotations_of (^^y)[4]) == ^^unsigned);
static_assert (type_of (annotations_of (^^y)[5]) == ^^long);
static_assert (type_of (annotations_of (^^y)[6]) == ^^unsigned);
static_assert (annotations_of_with_type (^^y, ^^int).size () == 1);
static_assert (type_of (annotations_of_with_type (^^y, ^^int)[0]) == ^^int);
static_assert (annotations_of_with_type (^^y, ^^long).size () == 2);
static_assert (type_of (annotations_of_with_type (^^y, ^^long)[0]) == ^^long);
static_assert (type_of (annotations_of_with_type (^^y, ^^long)[1]) == ^^long);
static_assert (annotations_of_with_type (^^y, ^^unsigned).size () == 3);
static_assert (type_of (annotations_of_with_type (^^y, ^^unsigned)[0]) == ^^unsigned);
static_assert (type_of (annotations_of_with_type (^^y, ^^unsigned)[1]) == ^^unsigned);
static_assert (type_of (annotations_of_with_type (^^y, ^^unsigned)[2]) == ^^unsigned);
static_assert (annotations_of_with_type (^^y, ^^const double).size () == 1);
static_assert (type_of (annotations_of_with_type (^^y, ^^double)[0]) == ^^double);
static_assert (annotations_of_with_type (^^y, ^^volatile double).size () == 0);
static_assert (annotations_of_with_type (^^y, ^^float).size () == 0);

int z;
static_assert (annotations_of (^^z).size () == 0);

struct V { int a, b, c; };
[[=1, =V { 2, 3, 4 }, =1.0]] int an;
static_assert (type_of (annotations_of (^^an)[0]) == ^^int);
static_assert (type_of (annotations_of (^^an)[1]) == ^^const V);
static_assert (type_of (annotations_of (^^an)[2]) == ^^double);

struct W { [[=1, =2L, =3U, =4UL]] int a; [[=V { 2, 3, 4 }, =1.0f]] int b; };
static_assert (type_of (annotations_of (^^W::a)[0]) == ^^int);
static_assert (type_of (annotations_of (^^W::a)[1]) == ^^long);
static_assert (type_of (annotations_of (^^W::a)[2]) == ^^unsigned);
static_assert (type_of (annotations_of (^^W::a)[3]) == ^^long unsigned);
static_assert (type_of (annotations_of (^^W::b)[0]) == ^^const V);
static_assert (type_of (annotations_of (^^W::b)[1]) == ^^float);

[[=1, =1.0f]] [[=2U]] extern int an2;
[[=3L, =4.0]] [[=5UL, =5UL]] extern int an2;
[[=6LL, =V { 7, 8, 9 }]] [[=6ULL, =6ULL]] int an2;
static_assert (annotations_of (^^an2).size () == 11);
static_assert (type_of (annotations_of (^^an2)[0]) == ^^int);
static_assert (type_of (annotations_of (^^an2)[1]) == ^^float);
static_assert (type_of (annotations_of (^^an2)[2]) == ^^unsigned);
static_assert (type_of (annotations_of (^^an2)[3]) == ^^long);
static_assert (type_of (annotations_of (^^an2)[4]) == ^^double);
static_assert (type_of (annotations_of (^^an2)[5]) == ^^unsigned long);
static_assert (type_of (annotations_of (^^an2)[6]) == ^^unsigned long);
static_assert (type_of (annotations_of (^^an2)[7]) == ^^long long);
static_assert (type_of (annotations_of (^^an2)[8]) == ^^const V);
static_assert (type_of (annotations_of (^^an2)[9]) == ^^unsigned long long);
static_assert (type_of (annotations_of (^^an2)[10]) == ^^unsigned long long);

namespace [[=1, =2U]] [[=3L, =4.0]] N
{
  static_assert (annotations_of (^^N).size () == 4);
  static_assert (type_of (annotations_of (^^N)[0]) == ^^int);
  static_assert (type_of (annotations_of (^^N)[1]) == ^^unsigned);
  static_assert (type_of (annotations_of (^^N)[2]) == ^^long);
  static_assert (type_of (annotations_of (^^N)[3]) == ^^double);
}

namespace [[=5.0f]] [[=6LL]] N
{
  static_assert (annotations_of (^^N).size () == 6);
  static_assert (type_of (annotations_of (^^N)[0]) == ^^int);
  static_assert (type_of (annotations_of (^^N)[1]) == ^^unsigned);
  static_assert (type_of (annotations_of (^^N)[2]) == ^^long);
  static_assert (type_of (annotations_of (^^N)[3]) == ^^double);
  static_assert (type_of (annotations_of (^^N)[4]) == ^^float);
  static_assert (type_of (annotations_of (^^N)[5]) == ^^long long);
}

struct F { int f; };
struct G { int g; };
struct H { int h; };
struct I : [[=1U, =2L]] [[=3]] F, [[=4ULL]] G, H { int i; };

constexpr auto ctx = access_context::unchecked ();
static_assert (annotations_of (bases_of (^^I, ctx)[0]).size () == 3);
static_assert (type_of (annotations_of (bases_of (^^I, ctx)[0])[0]) == ^^unsigned);
static_assert (type_of (annotations_of (bases_of (^^I, ctx)[0])[1]) == ^^long);
static_assert (type_of (annotations_of (bases_of (^^I, ctx)[0])[2]) == ^^int);
static_assert (annotations_of (bases_of (^^I, ctx)[1]).size () == 1);
static_assert (type_of (annotations_of (bases_of (^^I, ctx)[1])[0]) == ^^unsigned long long);
static_assert (annotations_of (bases_of (^^I, ctx)[2]).size () == 0);

struct J { int j; };
struct K { int k; };
struct L { int l; };
template <typename ...T>
struct M : [[=1]] F, G, [[=2U, =V { 1, 2, 3 }]] [[=3LL]] T... { int m; };
static_assert (bases_of (^^M <>, ctx).size () == 2);
static_assert (annotations_of (bases_of (^^M <>, ctx)[0]).size () == 1);
static_assert (type_of (annotations_of (bases_of (^^M <>, ctx)[0])[0]) == ^^int);
static_assert (annotations_of (bases_of (^^M <>, ctx)[1]).size () == 0);
static_assert (bases_of (^^M <J, K, L>, ctx).size () == 5);
static_assert (annotations_of (bases_of (^^M <J, K, L>, ctx)[0]).size () == 1);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[0])[0]) == ^^int);
static_assert (annotations_of (bases_of (^^M <J, K, L>, ctx)[1]).size () == 0);
static_assert (annotations_of (bases_of (^^M <J, K, L>, ctx)[2]).size () == 3);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[2])[0]) == ^^unsigned);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[2])[1]) == ^^const V);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[2])[2]) == ^^long long);
static_assert (annotations_of (bases_of (^^M <J, K, L>, ctx)[3]).size () == 3);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[3])[0]) == ^^unsigned);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[3])[1]) == ^^const V);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[3])[2]) == ^^long long);
static_assert (annotations_of (bases_of (^^M <J, K, L>, ctx)[4]).size () == 3);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[4])[0]) == ^^unsigned);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[4])[1]) == ^^const V);
static_assert (type_of (annotations_of (bases_of (^^M <J, K, L>, ctx)[4])[2]) == ^^long long);

template <auto ...V>
consteval auto
qux ()
{
  [[=1, =V..., =2]] int an;
  return ^^an;
}

static_assert (annotations_of (qux <> ()).size () == 2);
static_assert (annotations_of (qux <3, 4, 5> ()).size () == 5);
static_assert (annotations_of (qux <V { 1, 2, 3 }, V { 2, 3, 4 }> ()).size () == 4);
