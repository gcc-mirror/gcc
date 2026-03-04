// PR c++/123866
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct [[=1, =2]] A {};
using B [[=3, =4, =5]] = A;
typedef A C [[=6, =7, =8, =9]];
using D [[=10, =11, =12, =13, =14]] = const int;
typedef volatile int E [[=15, =16, =17, =18, =19, =20]];

static_assert (annotations_of (^^A).size () == 2);
static_assert (annotations_of (^^B).size () == 3);
static_assert (annotations_of (dealias (^^B)).size () == 2);
static_assert (annotations_of (^^C).size () == 4);
static_assert (annotations_of (dealias (^^C)).size () == 2);
static_assert (annotations_of (^^D).size () == 5);
static_assert (annotations_of (dealias (^^D)).size () == 0);
static_assert (annotations_of (^^E).size () == 6);
static_assert (annotations_of (dealias (^^E)).size () == 0);
