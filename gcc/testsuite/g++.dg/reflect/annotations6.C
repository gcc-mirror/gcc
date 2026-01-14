// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::annotations_of.

#include <meta>

using namespace std::meta;

extern int v;

consteval std::size_t
foo ()
{
  return annotations_of (^^v).size ();
}

static_assert (foo () == 0);
[[=1]] [[=2]] extern int v;
static_assert (foo () == 2);
[[=3, =4, =5]] extern int v;
static_assert (foo () == 5);
[[=6]] extern int v;
static_assert (foo () == 6);
[[=6]] int v;
static_assert (foo () == 7);
