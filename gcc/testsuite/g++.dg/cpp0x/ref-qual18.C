// PR c++/71784
// { dg-do compile { target c++11 } }

template<typename T> struct A {
  template<typename U> void f(U const&) & { }
  template<typename U> void f(U const&) && { }
};

template void A<int>::f<int>(int const&) &;
template void A<float>::f<int>(int const&) &&;

template<typename T> struct B {
  void f(int const&) & { }
  void f(int const&) && { }
};

template void B<int>::f(int const&) &;
template void B<float>::f(int const&) &&;
