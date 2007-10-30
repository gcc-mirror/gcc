// { dg-options "-std=c++0x" }
int x[5];

template<int M, int N, int (&... p)[N]> struct A;

template<int M> struct A<M,5,x> {};

A<0,5,x> a;  
