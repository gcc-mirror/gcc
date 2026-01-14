// { dg-do compile { target c++26 } }
// { dg-options "-freflection" }
// Test std::meta::alignment_of.

#include <meta>

using namespace std::meta;

struct S
{
  char a;
  int b;
};
struct [[gnu::packed]] V
{
  char a;
  long long f;
  int *h;
  S i;
} v;

static_assert (alignment_of (^^v) == 1);
static_assert (alignment_of (^^V) == alignof (V));
static_assert (alignment_of (^^V::a) == 1);
static_assert (alignment_of (^^V::f) == 1);
static_assert (alignment_of (^^V::h) == 1);
static_assert (alignment_of (^^V::i) == 1);
