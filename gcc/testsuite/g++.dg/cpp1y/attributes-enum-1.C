// PR c/47043
// { dg-do compile { target c++14 } }

class C
{
public:
  enum Foo
  {
    T,
    U [[deprecated("unused")]],
    V
  };
};

template<typename Tp>
  class D
  {
  public:
    enum Bar
    {
      X,
      Y [[deprecated("unused")]],
      Z
    };
  };

int
f (int i)
{
  auto j = C::U; // { dg-warning ".U. is deprecated" }

  auto k = D<int>::Y; // { dg-warning ".Y. is deprecated" }

  return i;
}
