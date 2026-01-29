// PR c++/123866
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct A
{
  [[=1, =2]] void foo () {}
};

static_assert (annotations_of (^^A::foo).size () == 2);
