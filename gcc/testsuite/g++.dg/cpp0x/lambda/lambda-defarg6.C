// PR c++/58597
// { dg-do compile { target c++11 } }

template<typename> struct A
{
  template<typename T> A(T, int = []{ return 0; }()) {}
};

A<int> a = 0;
