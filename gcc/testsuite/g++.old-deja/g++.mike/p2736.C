// { dg-do run  }
// This is a poor test case, it is meant to ensure that function local
// statics are destroyed at the right time.  See PR 2736 for details.
// prms-id: 2736

#include <stdlib.h>

int count;

struct A {
  int which;
  A(int i) :which(i) {
    // printf("ctor %x\n", this);
  }
  ~A() {
    // printf("dtor %x\n", this);
    if (++count != which)
      abort ();
    }
};

void
foo() {
  static A a(1);
}

A a(2);

int main() {
  foo();
}
