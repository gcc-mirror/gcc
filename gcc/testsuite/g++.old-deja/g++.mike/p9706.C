// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*
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
