// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

#include <exception>
#include <stdlib.h>

using std::uncaught_exception;
class A {
public:
  ~A() {
    if (uncaught_exception ())
      exit (0);
  }
};

int main() {
  if (uncaught_exception ())
    return 1;
  try {
    throw "";
  } catch (...) {
    if (uncaught_exception ())
      return 1;
  }
  if (uncaught_exception ())
    return 1;
  try {
    A a;
    throw "";
  } catch (...) {
    return 1;
  }
  return 1;
}
