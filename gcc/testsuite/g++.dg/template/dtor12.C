// PR c++/101740

template<template<class> class T, class U>
struct Test{
    void fun(){
        T<U> d;
        d.~GG<int>();  // #1
    }
};

template<class >
struct GG {};

int
main ()
{
  Test<GG, int> b;
  b.fun();
}
