// PR c++/124399
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template <class T>
  struct [[=42]] D { };

constexpr std::meta::info a1 = annotations_of (^^D <int>)[0];
constexpr std::meta::info a2 = annotations_of (^^D <char>)[0];
static_assert (a1 != a2);
static_assert (constant_of (a1) == constant_of (a2));

[[=1]] int x, y;
static_assert (annotations_of (^^x)[0] == annotations_of (^^y)[0]);
int z [[=2]], w [[=2]];
static_assert (annotations_of (^^z)[0] != annotations_of (^^w)[0]);
[[=3]] int u [[=3]], v [[=3]];
static_assert (annotations_of (^^u)[0] == annotations_of (^^v)[0]);
static_assert (annotations_of (^^u)[1] != annotations_of (^^v)[1]);
static_assert (annotations_of (^^u)[0] != annotations_of (^^u)[1]);
static_assert (annotations_of (^^v)[0] != annotations_of (^^v)[1]);
