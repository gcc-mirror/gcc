// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>
using namespace std::meta;

struct B {
  int b;
};
struct D : B { };

template<info R>
constexpr int
f ()
{
  D d = {42};
  B& b = d.[:R:];
  return b.b;
}
static_assert (f<bases_of(^^D, access_context::current ())[0]> () == 42);

template<info R>
constexpr int
g ()
{
  D d = {42};
  B *bp = &d.[:R:];
  return bp->b;
}
static_assert (g<bases_of(^^D, access_context::current ())[0]> () == 42);
