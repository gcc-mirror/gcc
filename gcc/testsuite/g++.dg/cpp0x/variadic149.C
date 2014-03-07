// PR c++/60248
// { dg-do compile { target c++11 } }
// { dg-options "-g -fabi-version=2" }

template<int...> struct A {};

template<> struct A<0>
{
  typedef enum { e } B;
};

A<0> a;
