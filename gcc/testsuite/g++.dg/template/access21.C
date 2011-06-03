// PR c++/48884

class X
{
  static const int I = 42;
  friend struct Y;
};

template <int I> struct A { };

struct Y
{
  template <typename T>
  static A<T::I> f(T t)
  {
    return A<T::I>();
  }
};

int main()
{
  Y::f(X());
}
