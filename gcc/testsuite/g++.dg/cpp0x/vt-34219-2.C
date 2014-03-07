// { dg-do compile { target c++11 } }
template<template<typename... T> class Comp, typename... T> void f( T... Value)
{
  static_assert( Comp<T>::value > 0, "" ); // { dg-error "parameter packs|T" }
}

template<template<typename... T> class Comp, typename... T> void g( T... Value)
{
  static_assert( Comp<T...>::value > 0, "" );
}

template <typename... T>
struct Foo
{
        static const int value=1;
};

int main()
{
        f<Foo>( 2 );
        g<Foo>( 2 );
}
