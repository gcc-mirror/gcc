// PR c++/116913
// { dg-do compile { target c++11 } }

namespace ns {
  struct c {};
  using d = int;
}

using ns::c;
using ns::d;

using c = ns::c;
using d = ns::d;
