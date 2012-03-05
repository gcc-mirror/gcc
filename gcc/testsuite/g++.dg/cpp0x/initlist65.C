// Core 1270
// { dg-options -std=c++11 }

struct A
{
  int i[2];
};

A f() { return {1,2}; }
