// PR c++/38684
// { dg-options "-std=c++0x" }

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
