// PR c++/13157

namespace aa
{
  double abs(double);
  long double abs(long double);
}

namespace fu
{
  template <class T>
  struct X
  {};
  
  template <class T>
  X<T> test(X<T> x)
  {
    using ::aa::abs;
    return abs(x);
  }
  
  template <class T>
  X<T> abs(X<T>);
  
  X<int> x;
  X<int> z = test(x);
}

