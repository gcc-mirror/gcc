// PR c++/60177

template<class> struct Base { };

struct Derived : Base<void> { };

template<template<typename> class TT, typename T>
void func (TT<T>) { }

int main () {
  func (Derived ());
}
