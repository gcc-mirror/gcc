// PR c++/63306
// { dg-do compile { target c++11 } }

template <typename...>
class A;

class B
{
  B (const int &, const A<int, int> &);
};

B::B (const int &, const A<int, int> &)
{
}
