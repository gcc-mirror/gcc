// PR c++/53581

template<class A, int M, int N>
class Child;

template<class A, int M, int N>
class Base
{
public:
  Child<A, M, N> operator-(const Base<A, M, N> &m) const
  {
    Child<A, M, N> diff;
    return diff;
  }

  A test() const
  {
    return 0;
  }

private:
  A values[M * N];
};

template<class A, int N>
class Ops
{
public:
  virtual ~Ops() {}

  bool bar() const
  {
    Child<A, N, N> mat;
    return (*static_cast<const Child<A, N, N>*>(this) - mat).test();
  }
};


template<class A, int N>
class Child<A, N, N> : public Base<A, N, N>, public Ops<A, N> {};

class ImageWarp
{
  bool bar() const
  {
    return foo.bar();
  }

  Child<float, 3, 3> foo;
};
