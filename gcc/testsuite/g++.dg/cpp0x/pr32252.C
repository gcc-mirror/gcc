// { dg-do compile { target c++11 } }
int x[5];

template<int M, int N, int (&... p)[N]> struct A;

template<int M> struct A<M,5,x> {};

A<0,5,x> a;  
