#include <stddef.h>

template <bool> struct StaticAssert;
template <> struct StaticAssert<true> {};

struct MyPOD
{
  int a; int b; int c; 
};

StaticAssert<(offsetof(MyPOD, a) == 0)> s;
