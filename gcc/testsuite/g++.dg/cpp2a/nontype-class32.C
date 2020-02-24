// PR c++/88819
// { dg-do compile { target c++2a } }

template<typename T, template<T> class TT, class R = TT <0>> struct A 
{
  template<R> struct B {};
};
template<int> struct C {};

A<int, C, C<0>> a;
