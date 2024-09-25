// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
#include<iostream>
#include<cstddef>

struct A {
  A() {
    std::cerr<<"A constructing\n";
    throw 1;
  }
  void *operator new(size_t sz) {
    std::cerr<<"A allocated\n";
    return ::operator new(sz);
  }
  void operator delete (void *p) {
    std::cerr<<"A deleted\n";
    ::operator delete (p);
  }
};

int main() {
  try {
     new A();
  } catch (...) {
  }
}



