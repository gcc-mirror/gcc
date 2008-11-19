// PR c++/37563

struct A {};

template<int> struct Traits
{
  typedef void X;
};

template<> struct Traits<0>
{
  typedef A X;
};

template<int N> struct B
{
  typedef typename Traits<N>::X Y;

  void foo(Y y)
  {
    y.Y::A::~A();
  }
};
