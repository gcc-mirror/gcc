// Origin: grg at ai dot mit dot edu
// Build don't link:

class A;
template<template<class Ignored> class base> class C : 
                                      public base<A> {
 public:
    C(A& newa) : base<A>(newa) {}
};
