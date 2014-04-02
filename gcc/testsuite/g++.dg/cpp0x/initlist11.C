// PR c++/38684
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct Y {};

struct X : Y { 
  X(std::initializer_list<int>) {}
}; 

struct A { 
  X v;
};

int main() {
  A a{ {1,2,3} };
}
