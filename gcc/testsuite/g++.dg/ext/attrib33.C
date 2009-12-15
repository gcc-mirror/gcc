// PR c++/35546
// { dg-do compile }
// { dg-options "-g" }

template <int N>
struct T
{
  void foo (char const * ...) __attribute__ ((format (printf,2,3)));
};

template struct T<3>;

template <typename T>
struct U
{
  typedef T __attribute__((mode (SI))) V;
};

U<int>::V v;
