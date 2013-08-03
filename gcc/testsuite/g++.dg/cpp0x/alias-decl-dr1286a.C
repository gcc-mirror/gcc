// DR 1286
// { dg-do compile { target c++11 } }

template <class,class> struct same;
template <class T> struct same<T,T> {};

template <class,class> struct different {};
template <class T> struct different<T,T>;

template<typename T, typename U = T> struct A;
template<template <class...> class> struct X;

// equivalent to A
template<typename V, typename W>
using B = A<V, W>;

same<X<A>,X<B>> s1;

// not equivalent to A: not all parameters used
template<typename V, typename W>
using C = A<V>;

different<X<A>,X<C>> d1;

// not equivalent to A: different number of parameters
template<typename V>
using D = A<V>;

different<X<A>,X<D>> d2;

// not equivalent to A: template-arguments in wrong order
template<typename V, typename W>
using E = A<W, V>;

different<X<A>,X<E>> d3;

// equivalent to A: default arguments not considered
template<typename V, typename W = int>
using F = A<V, W>;

same<X<A>,X<F>> s2;

// equivalent to A and B
template<typename V, typename W>
using G = A<V, W>;

same<X<A>,X<G>> s3;
same<X<B>,X<G>> s3b;

// equivalent to E
template<typename V, typename W>
using H = E<V, W>;

same<X<E>,X<H>> s4;

// not equivalent to A: argument not identifier
template<typename V, typename W>
using I = A<V, typename W::type>;

different<X<A>,X<I>> d4;
