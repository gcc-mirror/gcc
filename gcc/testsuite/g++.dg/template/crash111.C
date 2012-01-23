// PR c++/51398

template<void, int N> struct A   // { dg-error "not a valid type" }
{
  static const int i = N;
};
