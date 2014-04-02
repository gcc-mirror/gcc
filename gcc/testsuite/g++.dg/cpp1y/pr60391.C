// PR c++/60391
// { dg-do compile { target c++1y } }
// { dg-options "" }

namespace N
{
  int operator"" _X(auto) {} // { dg-error "invalid" }
}

namespace N {}
