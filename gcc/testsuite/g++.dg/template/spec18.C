// PR c++/17936

template<int, int N> struct A
{
  void foo();
};

template<int N> struct A<1, N>
{
  void foo();
};

template<> void A<1, 2>::foo();
