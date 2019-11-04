// PR c++/60391
// { dg-do compile { target c++14 } }

namespace N
{
  int operator"" _X(auto) {} // { dg-error "auto|invalid" }
}

namespace N {}
