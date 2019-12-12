// { dg-do compile { target c++11 } }
// { dg-options "-w" }

namespace A
{
  namespace B // { dg-bogus "inline namespace" }
  {
  }

  using namespace B __attribute__ ((strong)); // { dg-bogus "no longer supported" }
}
