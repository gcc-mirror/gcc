// Build don't link:
#include<iostream.h>
#include<stddef.h>

struct A {
  A() {
    cerr<<"A constructing\n";
    throw 1;
  }
  void *operator new(size_t sz) {
    cerr<<"A allocated\n";
    return ::operator new(sz);
  }
  void operator delete (void *p) {
    cerr<<"A deleted\n";
    ::operator delete (p);
  }
};

int main() {
  try {
     new A();
  } catch (...) {
  }
}
