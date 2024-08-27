#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++11-extensions"

template <class T> struct A
{
  void f() { }
};

extern template class A<int>;
extern template void A<char>::f();
