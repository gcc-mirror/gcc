// { dg-do compile { target c++11 } }

namespace A
{
  namespace B // { dg-message "inline namespace" }
  {
  }

  using namespace B __attribute__ ((strong)); // { dg-warning "no longer supported" "" }
}
