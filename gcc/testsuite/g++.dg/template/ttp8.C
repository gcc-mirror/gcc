// { dg-do compile }
// Contributed by: Niall Douglas <s_gccbugzilla at netprod dot com>
// PR c++/14284: Failure to select specialization 

template<typename> struct S; 
template<template<class> class> struct I {}; 
 
template<class, int> struct Match; 
 
template<template<class> class C> 
struct Match<I<C>, 0> {}; 
 
template<template<class> class C, int i> 
struct Match<I<C>, i>; 
 
Match<I<S>, 0> v; 
