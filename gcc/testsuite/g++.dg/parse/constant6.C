// PR c++/19883

template<typename T> struct A
{
  static const T i = 1;
  char a[int(i)];
};

template<int> struct B {};

template<typename T> struct C
{
  static const T i = 2;
  B<int(i)> a;
};

template< typename T, T N >
struct integral_c
{
  static const T value = N;

  typedef integral_c< T, static_cast<T>((value + 1)) > next;
};
