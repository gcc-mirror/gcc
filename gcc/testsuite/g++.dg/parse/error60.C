// PR c++/84537
// { dg-do compile }

namespace N
{
  template<int> struct A {};
}

N::template A<> a; // { dg-error "" }
