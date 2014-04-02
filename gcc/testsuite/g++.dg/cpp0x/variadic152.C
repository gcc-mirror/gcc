// PR c++/54440
// { dg-do compile { target c++11 } }

template <class...T> struct A
{
  template <template <T> class... TP, class U> struct B { };
};

template <int I> struct C { };
template <char C> struct D { };

A<int,char>::B<C,D,float> b;
