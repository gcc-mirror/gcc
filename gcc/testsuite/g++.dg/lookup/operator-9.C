// PR c++/123601
// { dg-do compile { target c++20 } }

#include <compare>

struct A {
  bool operator==(int);
  int operator<=>(int);
};

template<class T>
void f() {
  A a;
  (void)(0 <=> a);
}

template void f<int>();
