// PR c++/48281
// { dg-options "-O2" }
// { dg-do run { target c++11 } }

#include <initializer_list>

typedef std::initializer_list<int>  int1;
typedef std::initializer_list<int1> int2;
static int2 ib = {{42,2,3,4,5},{2,3,4,5,1},{3,4,5,2,1}};

int main()
{
  return *(ib.begin()->begin()) != 42;
}
