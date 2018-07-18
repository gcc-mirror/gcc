// PR c++/81853
// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace N {
  enum { i };
}

int g ()
{
  constexpr int j = ({ using namespace N; i; });
  return j;
}
