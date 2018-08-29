// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

#include <typeinfo>

class A {
  int space;
};
class B {
public:
  int data;
  B(int i) : data(i) {
  }
};
class D : public A, public B {
public:
  D(int i) : B(i) {
  }
} d(42);

int
main() {
  try {
    throw &d;
  } catch (B* b) {
    if (b->data == 42)
      return 0;
  }
  return 1;
}
