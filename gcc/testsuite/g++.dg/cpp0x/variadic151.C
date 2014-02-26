// PR c++/54440
// { dg-do compile { target c++11 } }

template <class...T> struct A
{
  template <T... t, class U> struct B { };
};

A<int,char>::B<42,'a',float> b;
