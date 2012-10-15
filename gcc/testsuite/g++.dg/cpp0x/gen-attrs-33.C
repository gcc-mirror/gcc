// PR c++/35546
// { dg-do compile { target c++11 } }
// { dg-options "-g" }

template <int N>
struct T
{
  void foo [[gnu::format (printf,2,3)]] (char const * ...);
};

template struct T<3>;

template <typename T>
struct U
{
  typedef T V [[gnu::mode (SI)]];
};

U<int>::V v;
