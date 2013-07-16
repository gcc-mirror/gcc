// PR c++/57222
// { dg-require-effective-target c++11 }

template <template <typename T> class Templ>
using Bool = Templ<bool>;

template <typename T>
class Foo {
private:
public:
    template<template<typename U> class Templ>
    void method(Bool<Templ> boolTempl);
};

template <typename T>
template <template <typename U> class Templ>
void Foo<T>::method(Bool<Templ> boolTempl) {
}

int main() {
    Foo<char> foo;
    return 0;
}
