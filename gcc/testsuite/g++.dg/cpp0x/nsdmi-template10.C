// PR c++/60999
// { dg-do compile { target c++11 } }

struct B
{
  template<int N, int M>
  struct A;

  template<int M>
  struct A<1, M>
  {
    int X = M;
  };
};
