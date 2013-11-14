// PR c++/57887
// { dg-do compile { target c++11 } }

struct B
{
  template<int N>
  struct A
  {
    int X = N;
  };
};

template<int M>
struct C
{
  int Y = M;

  template<int N>
  struct A
  {
    int X = N;
    int Y = M;
  };
};
