// PR c++/8928
// Origin: <sebor@roguewave.com>
// { dg-do compile }

namespace N
{

template <typename T, typename U> struct A {};
typedef A<int, int> B;

}

N::B<int, int> a; // { dg-error "" }
