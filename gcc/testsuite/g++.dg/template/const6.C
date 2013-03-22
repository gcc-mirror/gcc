// PR c++/56684

template < int T > struct S
{
  static const int Ti = T;
  S() { 1 << Ti; }
};
