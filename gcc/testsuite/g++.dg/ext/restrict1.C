// PR c++/6392
// { dg-do compile }

struct A
{
  int* __restrict__ data[10];
};
