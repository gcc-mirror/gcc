// PR c++/50303
// { dg-do compile { target c++11 } }

template<typename Interface>
struct A1 {
};

template<template<class I> class... Actions>
void g2() {
  g2<Actions...>();
}

int main()
{
  g2<A1>();
}
