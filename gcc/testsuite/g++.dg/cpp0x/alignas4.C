// PR c++/59012
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "align 8" { target x86_64-*-*-gnu } } }

template <class... T>
struct A
{
  alignas(T...) char t;
};

A<int,double> a;

template <class... T>
struct A2
{
  [[gnu::aligned (alignof (T))...]] char t;
};

A2<int,double> a2;
