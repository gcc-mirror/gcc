// { dg-options "-std=c++0x" }

template<class T> struct S0 {};
template<class T> using AS0 = S0<T>;

template<template<class> class TT>
void f(TT<int>);

template class AS0<char>;

void
foo()
{
  AS0<int> a;
  f(a);
}

template<class T, class U> struct Vector{};
template<class T> struct Alloc {};

template<class T> using Vec = Vector<T, Alloc<T> >;

template<class T> void g(Vector<T, Alloc<T> >);

template<template<class T> class TT> void h(TT<int>); // { dg-error "provided for" }

void
bar()
{
  Vec<int> a;
  g(a);
  h(a); // { dg-error "no matching function|wrong number of template arguments" }
}
