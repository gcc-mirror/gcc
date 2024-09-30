/* PR c++/91334.  */
/* { dg-do compile } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib size_t" { ! hostedlib } } */

#include <new>
#include <stdlib.h>

struct A {
  A() { throw 0; }
  void* operator new(size_t size, double = 0.0) { return ::operator new(size);}
  void operator delete(void* p, double) { exit(0); }
  void operator delete(void* p) { abort(); }
};

int main() { try { new A; } catch(...) {} }
