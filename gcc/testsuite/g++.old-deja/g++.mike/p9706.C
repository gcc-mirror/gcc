// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }
// { dg-skip-if "requires hosted libstdc++ for stdlib size_t" { ! hostedlib } }
// prms-id: 9706

#include <stdlib.h>

int count, acount;

void *operator new(size_t sz) { ++count; return malloc (sz); }
void operator delete(void *p) throw() { --count; free (p); }

class A {
public:
  A() { ++acount; }
  A(const A&) { ++acount; }
  ~A() { --acount; }
};

int main() {
  int i;

  // The standard library may have called new and/or delete during
  // startup, so we have to reset the counter here.
  count = 0;

  for( i = 0; i < 10; i++ ) {
    try {
      throw A();
    }
    catch (A& a) {
    }
  }
  if (acount)
    return 1;
  if (count)
    return 2;
}
