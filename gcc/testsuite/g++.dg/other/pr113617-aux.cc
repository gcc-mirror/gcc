// PR rtl-optimization/113617
// { dg-do link { target { c++17 && c++14_down } } }

#include "pr113617.h"

void qux() {
  A<long long> a;
  a.foo(0, 0);
}
