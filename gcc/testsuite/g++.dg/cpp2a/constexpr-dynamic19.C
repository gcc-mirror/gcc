// PR c++/120620
// { dg-do compile }

#include <cxxabi.h>

struct A* a;

void f() {
  void* const p = abi::__dynamic_cast(&a, 0, 0, 42);
}
